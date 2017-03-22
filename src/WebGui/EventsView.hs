{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module EventsView where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import GHCJS.DOM.Element as E
import GHCJS.DOM.Document
import GHCJS.DOM.Node (getOwnerDocument)
import GHCJS.DOM.EventM as DE (on, stopPropagation)

import Reflex
import Reflex.Dom hiding (display)

import Control.Monad.Identity
import Clay as C hiding (col, div, id, start, end, focus, dt)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Control.Error

import TsEvent
import Communication
import Common

newtype PikadayPicker = PikadayPicker { unPicker :: JSVal }

foreign import javascript unsafe
    "$r = new Pikaday({firstDay: 1,onSelect: function(picker) {\
        \$2(picker.toString()) }}); $1.appendChild($r.el)"
    _initPikaday :: JSVal -> Callback (JSVal -> IO ()) -> IO JSVal

initPikaday :: JSVal -> Callback (JSVal -> IO ()) -> IO PikadayPicker
initPikaday = fmap (fmap PikadayPicker) . _initPikaday

foreign import javascript unsafe
    "$1.setDate($2, true)" -- the true is to prevent from triggering "onSelect"
    _pickerSetDate :: JSVal -> JSVal -> IO ()

pickerSetDate :: PikadayPicker -> Day -> IO ()
pickerSetDate picker day = _pickerSetDate (unPicker picker) =<< toJSVal (showGregorian day)

foreign import javascript unsafe "$1.hide()" _pickerHide :: JSVal -> IO ()
pickerHide :: PikadayPicker -> IO ()
pickerHide = _pickerHide . unPicker

foreign import javascript unsafe "$1.show()" _pickerShow :: JSVal -> IO ()
pickerShow :: PikadayPicker -> IO ()
pickerShow = _pickerShow . unPicker

eventsView :: MonadWidget t m => Dynamic t ActiveView -> m ()
eventsView activeViewDyn = do
    let rootStyle = do
            flexGrow 1
            display flex
            flexDirection column
            overflow auto
    let attrsDyn = ffor activeViewDyn $ \curView ->
          attrStyleWithHideIf (curView /= ActiveViewEvents) rootStyle
    elDynAttr "div" attrsDyn $ eventsViewContents

eventsViewContents :: MonadWidget t m => m ()
eventsViewContents = do
    rec
        -- initialize on yesterday, because it's finished i can cache it.
        today <- liftIO getToday
        let initialDay = addDays (-1) today
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        loadRecordsEvent <- leftmost <$> sequence
            [pure $ updated curDate, fmap (const initialDay) <$> getPostBuild]
        responseEvt <- requestDayEvents loadRecordsEvent
        -- the leftmost makes sure that we reset the respDyn
        -- to loading state when loadRecordsEvent is triggered.
        respDyn <- holdDyn RemoteDataLoading $ leftmost
            [fmap (const RemoteDataLoading) loadRecordsEvent, responseEvt]
        displayWarningBanner respDyn
        curDate <- addDatePicker initialDay

    displayEvents respDyn

    -- display the progress indicator if needed.
    let holdAttrs = ffor respDyn $ \curEvt ->
          "id" =: "pleasehold" <> attrStyleHideIf (not $ isRemoteDataLoading curEvt)
    elDynAttr "div" holdAttrs $ text "Please hold..."
    return ()

displayEvents :: MonadWidget t m => Dynamic t (RemoteData FetchResponse) -> m ()
displayEvents respDyn = do
    let divStyle = do
            display flex
            flexGrow 1
            flexDirection row
            marginTop (px 20)
            overflow auto -- !!
    elStyle "div" divStyle $ do
        tableRes <- dyn (fmap eventsTable respDyn) >>= holdDyn (constDyn Nothing)
        -- uniqDyn to avoid blinking if you click on the already-selected event.
        let curEvtDyn = uniqDyn (join tableRes)
        void $ dyn $ fmap displayDetails curEvtDyn

requestDayEvents :: MonadWidget t m => Event t Day -> m (Event t (RemoteData FetchResponse))
requestDayEvents dayEvt = do
    let req reqUrl = xhrRequest "GET" ("/timesheet/" <> reqUrl) def
    asyncReq <- performRequestAsync (req . T.pack . showGregorian <$> dayEvt)
    return (fmap readRemoteData asyncReq)

getToday :: IO Day
getToday = do
    tz <- liftIO (getCurrentTimeZoneJS =<< getCurrentTime)
    curLocalTime <- liftIO (utcToLocalTime tz <$> getCurrentTime)
    return (localDay curLocalTime)

addDatePicker :: MonadWidget t m => Day -> m (Dynamic t Day)
addDatePicker initialDay = do
    rec
        -- update current day based on user actions
        curDate <- foldDyn ($) initialDay dayChangeEvt
        -- the date picker has position: absolute & will position itself
        -- relative to the nearest ancestor with position: relative.
        dayChangeEvt <-
            elStyle "table" (marginLeft $ px 10) $ el "tr" $
                col (text "Day to display:") *>
                col (displayPickerBlock initialDay curDate) <*
                col addPreloadButton
    return curDate

-- TODO display any errors occuring during the prefetching
addPreloadButton :: MonadWidget t m => m ()
addPreloadButton = do
    (btnElt, displayPreload) <- iconButton 16 "glyphicons-58-history"
    rec
        (dlgBody, dlgClose) <- buildModalBody displayPreload "Preload data"
            (PrimaryBtn "Preload") (constDyn "") (constDyn $ preloadDialog progressDyn)
        let preloadEvt = tagDyn (joinDyn $ dlgContentsDyn dlgBody) (dlgOkEvt dlgBody)
        let dayListEvt = fmap (uncurry daysRange) preloadEvt
        curCountDyn <- holdDyn 0 $ leftmost [length <$> dayListEvt, const 0 <$> displayPreload]
        daysFetchingQueueDyn <- foldDyn ($) [] $ leftmost
            [fmap const dayListEvt, fmap (const tail) $ daysFetchingQueueDoneEvt]
        progressDyn <- combineDyn (\lst cnt -> if cnt == 0 then 0 else (cnt - length lst)*100 `div` cnt)
            daysFetchingQueueDyn curCountDyn
        mCurDayToFetchDyn <- uniqDyn <$> mapDyn headZ daysFetchingQueueDyn
        daysFetchingQueueDoneEvt <- fetchDay mCurDayToFetchDyn
    let doneEvt = ffilter isNothing $ updated mCurDayToFetchDyn
    performEvent_ $ (const $ liftIO dlgClose) <$> doneEvt

fetchDay :: MonadWidget t m => Dynamic t (Maybe Day) -> m (Event t FetchResponse)
fetchDay mDayDyn = do
    fetched <- requestDayEvents $ fmapMaybe id $ updated mDayDyn
    return $ fmapMaybe fromRemoteData fetched

preloadDialog :: MonadWidget t m => Dynamic t Int -> m (Dynamic t (Day, Day))
preloadDialog percentDyn = do
    today <- liftIO getToday
    -- fetch from the first day of the previous month
    let prefetchStart = addGregorianMonthsClip (-1) $
                        (\(y,m,_) -> fromGregorian y m 1) $ toGregorian today
    let prefetchEnd = addDays (-1) today
    el "p" $ text "Navigating several days of data can get slow\
                  \ if you have to wait for each day to load separately."
    el "p" $ text "You can preload data for a certain time interval\
                  \ to minimize the waiting later."
    interval <- elStyle "table" (paddingBottom $ px 15) $ do
        rec dynStartDay <- el "tr" $ do
            col $ text "Pick a start date:"
            col $ do
                startDayChangeEvt <- displayPickerBlock prefetchStart dynStartDay
                foldDyn ($) prefetchStart startDayChangeEvt
        rec dynEndDay <- el "tr" $ do
            col $ text "Pick an end date:"
            col $ do
                endDayChangeEvt <- displayPickerBlock prefetchEnd dynEndDay
                foldDyn ($) prefetchEnd endDayChangeEvt
        combineDyn (,) dynStartDay dynEndDay
    progressWidget percentDyn
    return interval

progressWidget :: MonadWidget t m => Dynamic t Int -> m ()
progressWidget percentDyn = do
    attrsDyn <- forDyn percentDyn $ \percent ->
        ("class" =: "progress" <> "value" =: T.pack (show percent) <> "max" =: "100")
    elDynAttrStyle "progress" attrsDyn (height (px 50) >> paddingAll (px 15)) $
        dynText =<< mapDyn ((<> "%") . T.pack . show) percentDyn

daysRange :: Day -> Day -> [Day]
daysRange start end
    | start < end  = start : daysRange (addDays 1 start) end
    | start == end = [start]
    | otherwise    = []

displayPickerBlock :: MonadWidget t m => Day -> Dynamic t Day -> m (Event t (Day -> Day))
displayPickerBlock initialDay curDate = do
    rec
        let btnClass = "class" =: "btn-group" <> "data-toggle" =: "buttons"
        previousNextEvt <-
            elAttrStyle "div" btnClass (position relative) $ do
                previousDay <- fmap (const $ addDays (-1)) <$> snd <$>
                    smallIconButton "glyphicons-171-step-backward"
                createDateLabel curDate picker
                nextDay <- fmap (const $ addDays 1) <$> snd <$>
                    smallIconButton "glyphicons-179-step-forward"
                return (mergeWith (.) [previousDay, nextDay])
        (pickedDateEvt, picker) <- datePicker initialDay
    liftIO (pickerHide picker)
    return (mergeWith (.) [fmap const pickedDateEvt, previousNextEvt])

createDateLabel :: MonadWidget t m => Dynamic t Day -> PikadayPicker -> m ()
createDateLabel curDate picker = do
    rec
        cbDyn <- holdDyn True (leftmost [dayToggleEvt, pickerAutoCloseEvt])

        let labelClass = "class" =: "btn btn-secondary btn-sm"
        let labelStyle = do
                marginBottom (px 0)
                width (px 180)
                maxWidth (px 180)
        (label, _) <- elAttrStyle' "label" labelClass labelStyle $ do
            -- without the disabled, the event triggers twice with stopPropagation
            void $ checkboxView (constDyn $ "disabled" =: "disabled") cbDyn
            dynText =<< mapDyn (T.pack . formatTime defaultTimeLocale "%A, %F") curDate
        -- use stopPropagation so that I can catch the clicks on the body elsewhere
        -- and close the date picker when the user clicks elsewhere.
        e <- wrapDomEvent (_el_element label) (`on` E.click) DE.stopPropagation

        -- trigger day toggle event when the day button is pressed
        dayToggleEvt <- performEvent $ fmap (const $ liftIO $ do
            eltToggleClass (_el_element label) "active"
            (cn :: String) <- getClassName (_el_element label)
            return ("active" `isInfixOf` cn)) e

        -- close the datepicker & update its date on day change.
        pickerAutoCloseEvt <- performEvent $ fmap
            (\d -> liftIO $ do
                  eltStripClass (_el_element label) "active"
                  pickerSetDate picker d
                  return False) (updated curDate)

        -- close the date picker on any click anywhere else.
        (Just doc) <- getOwnerDocument (_el_element label)
        (Just body) <- liftIO (getBody doc)
        bodyElt <- wrapElement defaultDomEventHandler (toElement body)
        performEvent_ $ fmap (const $ liftIO $ do
                                   eltStripClass (_el_element label) "active"
                                   pickerHide picker) $ domEvent Click bodyElt

    -- open or close the datepicker when the user clicks on the toggle button
    performOnDynChange cbDyn $ \isActive ->
        liftIO $ (if isActive then pickerShow else pickerHide) picker

displayWarningBanner :: MonadWidget t m => Dynamic t (RemoteData FetchResponse) -> m ()
displayWarningBanner respDyn = do
    let basicAttrs = "class" =: "alert alert-warning alert-dismissible" <> "role" =: "alert"
    let getErrorTxt = \case
            RemoteData (FetchResponse _ errors@(_:_)) -> Just (T.pack $ intercalate ", " errors)
            RemoteDataInvalid msg -> Just msg
            _ -> Nothing
    errorTxtDyn <- mapDyn getErrorTxt respDyn

    let styleContents e = attrStyleWithHideIf (isNothing e) $ do
            width (pct 65)
            marginLeft auto
            marginRight auto
            flexShrink 0
    blockAttrs <- forDyn errorTxtDyn (\e -> basicAttrs <> styleContents e)
    elDynAttr "div" blockAttrs $ do
        elAttr "button" ("type" =: "button" <>
                         "class" =: "close" <>
                         "data-dismiss" =: "alert") $ do
            void $ elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
            elAttr "span" ("class" =: "sr-only") $ text "Close"
        elStyle "strong" (paddingRight $ px 7) $ text "Error"
        el "span" $ dynText =<< mapDyn (fromMaybe "") errorTxtDyn

eventsTable :: MonadWidget t m => RemoteData FetchResponse -> m (Dynamic t (Maybe TsEvent))
eventsTable (RemoteDataInvalid _) = return (constDyn Nothing)
eventsTable RemoteDataLoading = return (constDyn Nothing)
eventsTable (RemoteData (FetchResponse tsEvents _)) =
    elStyle "div" (width (px 500) >> flexShrink 0 >> overflow auto) $ do
        -- display: block is needed for overflow to work, http://stackoverflow.com/a/4457290/516188
        let tableStyle = display block >> overflow auto >> width (px 500)
        elAttrStyle "table" ("class" =: "table") tableStyle $ do
            rec
                -- need to tell to the individual showRecords which is the current
                -- record. Select the first record by default, if any, using headZ.
                events <- mapM (showRecord curEventDyn) tsEvents
                curEventDyn <- holdDyn (headZ tsEvents) $ leftmost $ fmap (fmap Just) events
            return curEventDyn

-- https://github.com/ghcjs/ghcjs-base/issues/16
-- passing long as string as i'm unsure how to pass longs.
foreign import javascript unsafe
    "-(new Date(parseInt($1)).getTimezoneOffset())" -- the offset is the wrong way...
    _getTimezoneOffsetMinsForDateMs :: JSVal -> IO Int
getTimezoneOffsetMinsForDateMs :: UTCTime -> IO Int
getTimezoneOffsetMinsForDateMs = (_getTimezoneOffsetMinsForDateMs <=< toJSVal) . show
    . (*1000) . (floor :: NominalDiffTime -> Integer) . utcTimeToPOSIXSeconds

getCurrentTimeZoneJS :: UTCTime -> IO TimeZone
getCurrentTimeZoneJS = fmap minutesToTimeZone . getTimezoneOffsetMinsForDateMs

absTop :: Double -> Css
absTop y = position absolute >> top (px y)

showRecord :: MonadWidget t m => Dynamic t (Maybe TsEvent) -> TsEvent -> m (Event t TsEvent)
showRecord curEventDyn tsEvt@TsEvent{..} = do
    rowAttrs <- forDyn curEventDyn $ \curEvt ->
        "class" =: if curEvt == Just tsEvt then "table-active" else ""
    (e, _) <- elDynAttr' "tr" rowAttrs $
        elStyle "td" (height (px 60) >> width (px 500) >> cursor pointer) $
            elStyle "div" (position relative) $
                recordsContents tsEvt
    return (const tsEvt <$> domEvent Click e)

recordsContents :: MonadWidget t m => TsEvent -> m ()
recordsContents tsEvt@TsEvent{..} = do
    let imgWidth = 42
    let divFlexSetup = do
            display flex
            justifyContent center
            alignItems center
            flexDirection column
    elStyle "div" (width (px imgWidth) >> divFlexSetup) $ do
        elAttrStyle "img" ("src" =: getGlyphiconUrl (T.pack eventIcon))
            (alignItems center) $ return ()
        let pluginNameStyle = do
                color gray
                fontSize (em 0.8)
                textAlign (alignSide sideCenter)
        elStyle "span" pluginNameStyle $ text (T.pack pluginName)
    let detailsDivStyle = do
            absTop 0
            left (px imgWidth)
            width (other $ "calc(100% - " <> C.value imgWidth <> "px)")
            marginLeft (px 5)
    elStyle "div" detailsDivStyle $ detailsDiv tsEvt

detailsDiv :: MonadWidget t m => TsEvent -> m ()
detailsDiv TsEvent{..} = do
    tz <- liftIO (getCurrentTimeZoneJS eventDate)
    let fixedWidthStyle w = do
            position absolute
            width (px w)
            maxWidth (px w)
    elStyle "b" (absTop 0 >> fontSize (em 1.1)) $ text $ T.pack $
        formatTime defaultTimeLocale "%R" $ utcToZonedTime tz eventDate
    elAttrStyle "span" ("class" =: "ellipsis") (fixedWidthStyle 400 >> absTop 20) $ text desc
    let extraInfoStyle = do
            textAlign (alignSide sideRight)
            right (px 0)
            fixedWidthStyle 360
    elAttrStyle "span" ("class" =: "ellipsis") extraInfoStyle $ text extraInfo

displayDetails :: MonadWidget t m => Maybe TsEvent -> m ()
displayDetails Nothing = return ()
displayDetails (Just TsEvent{..}) = do
    let divStyle = do
        flexGrow 1
        display flex
        flexDirection column
        overflow auto
        paddingAll (px 7)
    elStyle "div" divStyle $ do
        el "h3" $ text desc
        el "h5" $ ellipsizedText 100 extraInfo
        mapM_ buildIframe fullContents

ellipsizedText :: MonadWidget t m => Int -> Text -> m ()
ellipsizedText ln txt = if T.length txt > ln
                            then elAttr "span" ("title" =: txt) $
                                     text (T.take ln txt <> "...")
                            else text txt

buildIframe :: MonadWidget t m => Text -> m ()
buildIframe cts = do
    let iframeClass = "srcdoc" =: cts <>
            "frameBorder" =: "0" <>
            "width" =: "100%"
    let iframeStyle = flexGrow 1 >> minHeight (px 0) >> minWidth (px 0)
    elAttrStyle "iframe" iframeClass iframeStyle (return ())

datePicker :: MonadWidget t m => Day -> m (Event t Day, PikadayPicker)
datePicker initialDay = do
    let pickerStyle = width (px 250) >> position absolute >> zIndex 3
    (e, _) <- elStyle' "div" pickerStyle $ return ()
    (evt, evtTrigger) <- newTriggerEvent
    picker <- liftIO $ do
        cb <- syncCallback1 ContinueAsync $ \date -> do
            dateStr <- fromJSVal date
            case parsePikadayDate =<< dateStr of
                Nothing -> return ()
                Just dt -> evtTrigger dt
        picker <- initPikaday (unwrapElt e) cb
        pickerSetDate picker initialDay
        return picker
    return (evt, picker)

-- the format from pikaday is "Tue Dec 22 2015 00:00:00 GMT+0100 (CET)" for me.
parsePikadayDate :: String -> Maybe Day
parsePikadayDate = parseTimeM False defaultTimeLocale "%a %b %d %Y %X GMT%z (%Z)"
