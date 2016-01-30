{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, FlexibleContexts #-}

module EventsView where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element

import Reflex
import Reflex.Dom
import Reflex.Host.Class

import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import GHC.Generics
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.List
import Control.Error
import qualified Data.Map as Map

import Common

newtype PikadayPicker = PikadayPicker { unPicker :: JSRef Any }

foreign import javascript unsafe
    "$r = new Pikaday({firstDay: 1,onSelect: function(picker) { $2(picker.toString()) }}); $1.appendChild($r.el)"
    _initPikaday :: JSRef Element -> JSFun (JSString -> IO ()) -> IO (JSRef a)

initPikaday :: JSRef Element -> JSFun (JSString -> IO ()) -> IO PikadayPicker
initPikaday = fmap (fmap PikadayPicker) . _initPikaday

foreign import javascript unsafe
    "$1.setDate($2, true)" -- the true is to prevent from triggering "onSelect"
    _pickerSetDate :: JSRef a -> JSString -> IO ()

pickerSetDate :: PikadayPicker -> Day -> IO ()
pickerSetDate picker day = _pickerSetDate (unPicker picker) (toJSString $ showGregorian day)

foreign import javascript unsafe "$1.hide()" _pickerHide :: JSRef a -> IO ()
pickerHide :: PikadayPicker -> IO ()
pickerHide = _pickerHide . unPicker

foreign import javascript unsafe "$1.show()" _pickerShow :: JSRef a -> IO ()
pickerShow :: PikadayPicker -> IO ()
pickerShow = _pickerShow . unPicker

-- TODO share code with the server
-- instead of copy-pasting
data TsEvent = TsEvent
    {
        pluginName :: String,
        eventIcon :: String,
        eventDate :: UTCTime,
        desc :: T.Text,
        extraInfo :: T.Text,
        fullContents :: Maybe T.Text
    } deriving (Eq, Show, Generic)
instance FromJSON TsEvent

data FetchResponse = FetchResponse
    {
        fetchedEvents :: [TsEvent],
        fetchErrors :: [String]
    } deriving (Show, Generic)
instance FromJSON FetchResponse

eventsView :: MonadWidget t m => Dynamic t ActiveView -> m ()
eventsView activeViewDyn = do
    let cssPos = "overflow: hidden; position: absolute; bottom 0; right: 0; left: 0;"
    -- TODO the hardcoded 70px is no good! (height of navigation bar)
    attrsDyn <- forDyn activeViewDyn $ \curView ->
        styleWithHideIf (curView /= ActiveViewEvents) ("height: calc(100% - 70px);" <> cssPos)
    elDynAttr "div" attrsDyn $ do
        let req url = xhrRequest "GET" ("/timesheet/" ++ url) def
        rec
            -- TODO getPostBuild ... maybe when the tab is loaded instead?
            tz <- liftIO (getCurrentTimeZoneJS =<< getCurrentTime)
            curLocalTime <- liftIO (utcToLocalTime tz <$> getCurrentTime)
            -- initialize on yesterday, because it's finished i can cache it.
            let initialDay = addDays (-1) (localDay curLocalTime)
            loadRecordsEvent <- leftmost <$> sequence [pure $ updated curDate, fmap (const initialDay) <$> getPostBuild]
            asyncReq <- performRequestAsync (req <$> showGregorian <$> loadRecordsEvent)
            let responseEvt = fmap readRemoteData asyncReq
            -- the leftmost makes sure that we reset the respDyn to loading state when loadRecordsEvent is triggered.
            respDyn <- holdDyn RemoteDataLoading $ leftmost [fmap (const RemoteDataLoading) loadRecordsEvent, responseEvt]
            displayWarningBanner respDyn
            curDate <- addDatePicker initialDay

        -- TODO 60px is ugly (date picker height)
        elAttr "div" ("style" =: "display: flex; height: calc(100% - 60px); margin-top: 20px"
                      <> "overflow" =: "auto") $ do
            -- that's a mess. Surely there must be a better way to extract the event from the table.
            curEvtDyn <- joinDyn <$> (mapDyn eventsTable respDyn >>= dyn >>= holdDyn (constDyn Nothing))
            void $ mapDyn displayDetails curEvtDyn >>= dyn

        -- display the progress indicator if needed.
        holdAttrs <- forDyn respDyn $ \curEvt ->
            "id" =: "pleasehold" <> styleHideIf (not $ isRemoteDataLoading curEvt)
        elDynAttr "div" holdAttrs $ text "Please hold..."
        return ()

addDatePicker :: MonadWidget t m => Day -> m (Dynamic t Day)
addDatePicker initialDay = do
    rec
        -- update current day based on user actions
        curDate <- foldDyn ($) initialDay dayChangeEvt
        -- the date picker has position: absolute & will position itself
        -- relative to the nearest ancestor with position: relative.
        let col = elAttr "td" ("style" =: "padding: 5px")
        dayChangeEvt <-
            elAttr "table" ("style" =: "margin-left: 10px") $ el "tr" $ do
                col $ text "Day to display:"
                col $ displayPickerBlock initialDay curDate
    return curDate

displayPickerBlock :: MonadWidget t m => Day -> Dynamic t Day -> m (Event t (Day -> Day))
displayPickerBlock initialDay curDate = do
    rec
        previousNextEvt <-
            elAttr "div" ("class" =: "btn-group"
                          <> "data-toggle" =: "buttons"
                          <> "style" =: "position: relative") $ do
                let iconBtn icon = button' $
                      elAttr "img" ("src" =: getGlyphiconUrl icon
                                    <> "style" =: "height: 12px") $ return ()
                previousDay <- fmap (const $ addDays (-1)) <$>
                    iconBtn "glyphicons-171-step-backward"
                createDateLabel curDate picker
                nextDay <- fmap (const $ addDays 1) <$>
                    iconBtn "glyphicons-179-step-forward"
                return (mergeWith (.) [previousDay, nextDay])
        (pickedDateEvt, picker) <- datePicker initialDay
    liftIO (pickerHide picker)
    return (mergeWith (.) [fmap const pickedDateEvt, previousNextEvt])

createDateLabel :: MonadWidget t m => Dynamic t Day -> PikadayPicker -> m ()
createDateLabel curDate picker = do
    rec
        cbDyn <- holdDyn True (leftmost [dayToggleEvt, pickerAutoCloseEvt])

        (label, _) <- elAttr' "label" ("class" =: "btn btn-secondary btn-sm"
                                       <> "style" =: "margin-bottom: 0px; width: 180px; max-width: 180px") $ do
            void $ checkboxView (constDyn Map.empty) cbDyn
            dynText =<< mapDyn (formatTime defaultTimeLocale "%A, %F") curDate

        -- trigger day toggle event when the day button is pressed
        dayToggleEvt <- performEvent $ fmap (const $ liftIO $ do
            (cn :: String) <- elementGetClassName (_el_element label)
            return ("active" `isInfixOf` cn)) $ domEvent Click label

        -- close the datepicker & update its date on day change.
        pickerAutoCloseEvt <- performEvent $ fmap
            (\d -> liftIO $ do
                  eltStripClass (_el_element label) "active"
                  pickerSetDate picker d
                  return False) (updated curDate)

    -- open or close the datepicker when the user clicks on the toggle button
    performOnChange
        (\focus -> liftIO $ (if focus then pickerShow else pickerHide) picker)
        cbDyn

displayWarningBanner :: MonadWidget t m => Dynamic t (RemoteData FetchResponse) -> m ()
displayWarningBanner respDyn = do
    let basicAttrs = "class" =: "alert alert-warning alert-dismissible" <> "role" =: "alert"
    let getErrorTxt = \case
            RemoteData (FetchResponse _ errors@(_:_)) -> Just (intercalate ", " errors)
            RemoteDataInvalid msg -> Just msg
            _ -> Nothing
    errorTxtDyn <- mapDyn getErrorTxt respDyn

    let styleContents e = styleWithHideIf (isNothing e) "width: 65%; margin-left: auto; margin-right: auto"
    blockAttrs <- forDyn errorTxtDyn (\e -> basicAttrs <> styleContents e)
    elDynAttr "div" blockAttrs $ do
        elAttr "button" ("type" =: "button" <> "class" =: "close" <> "data-dismiss" =: "alert") $ do
            void $ elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
            elAttr "span" ("class" =: "sr-only") $ text "Close"
        elAttr "strong" ("style" =: "padding-right: 7px") $ text "Error"
        el "span" $ dynText =<< mapDyn (fromMaybe "") errorTxtDyn

eventsTable :: MonadWidget t m => RemoteData FetchResponse -> m (Dynamic t (Maybe TsEvent))
eventsTable (RemoteDataInvalid _) = return (constDyn Nothing)
eventsTable RemoteDataLoading = return (constDyn Nothing)
eventsTable (RemoteData (FetchResponse tsEvents _)) =
    elAttr "div" ("style" =: "width: 500px; height: 100%; flex-shrink: 0") $
        -- display: block is needed for overflow to work, http://stackoverflow.com/a/4457290/516188
        elAttr "table" ("class" =: "table" <> "style" =: "height: 100%; display:block; overflow:auto") $ do
            rec
                events <- mapM (showRecord curEventDyn) tsEvents
                curEventDyn <- holdDyn (headZ tsEvents) $ leftmost $ fmap (fmap Just) events
            return curEventDyn

-- https://github.com/ghcjs/ghcjs-base/issues/16
-- passing long as string as i'm unsure how to pass longs.
foreign import javascript unsafe
    "-(new Date(parseInt($1)).getTimezoneOffset())" -- the offset is the wrong way...
    _getTimezoneOffsetMinsForDateMs :: JSString -> IO Int
getTimezoneOffsetMinsForDateMs :: UTCTime -> IO Int
getTimezoneOffsetMinsForDateMs = _getTimezoneOffsetMinsForDateMs . toJSString . show
    . (*1000) . (floor :: NominalDiffTime -> Integer) . utcTimeToPOSIXSeconds

getCurrentTimeZoneJS :: UTCTime -> IO TimeZone
getCurrentTimeZoneJS = fmap minutesToTimeZone . getTimezoneOffsetMinsForDateMs

showRecord :: MonadWidget t m => Dynamic t (Maybe TsEvent) -> TsEvent -> m (Event t TsEvent)
showRecord curEventDyn tsEvt@TsEvent{..} = do
    rowAttrs <- forDyn curEventDyn $ \curEvt ->
        "class" =: if curEvt == Just tsEvt then "table-active" else ""
    tz <- liftIO (getCurrentTimeZoneJS eventDate)
    let fixedWidthStyle (w :: Int) = "position: absolute;" ++
           "width: " ++ show w ++ "px; max-width: " ++ show w ++ "px;"
    let absTop (y :: Int) = "position: absolute; top: " ++ show y ++ "px;"
    let imgWidth = "38px"
    (e, _) <- elDynAttr' "tr" rowAttrs $ do
        elAttr "td" ("style" =: "height: 60px; width: 500px;") $
            elAttr "div" ("style" =: "position: relative;") $ do
               let divFlexSetup = ";display: flex; justify-content: center;" ++
                     "align-items: center; flex-direction: column"
               elAttr "div" ("style" =: ("width: " ++ imgWidth <> divFlexSetup)) $ do
                   elAttr "img" ("src" =: (getGlyphiconUrl eventIcon)
                                 <> "style" =: "flex-item-align: center") $ return ()
                   let pluginNameStyle = "color: gray; font-size: 0.8em; text-align: center"
                   elAttr "span" ("style" =: pluginNameStyle) $ text pluginName
               let detailsDivStyle = absTop 0 ++ "left: " ++ imgWidth ++
                     "; width: calc(100% - " ++ imgWidth ++ "); margin-left: 5px"
               elAttr "div" ("style" =: detailsDivStyle) $ do
                   elAttr "b" ("style" =: (absTop 0 <> "font-size: 1.1em")) $ text $
                       formatTime defaultTimeLocale "%R" $ utcToZonedTime tz eventDate
                   elAttr "span" ("class" =: "ellipsis"
                                  <> "style" =: (fixedWidthStyle 400 <> absTop 20)) $ text_ desc
                   elAttr "span" ("class" =: "ellipsis"
                                  <> "style" =: ("right: 0px; " ++ fixedWidthStyle 365)) $ text_ extraInfo
    return (const tsEvt <$> domEvent Click e)

displayDetails :: MonadWidget t m => Maybe TsEvent -> m ()
displayDetails Nothing = return ()
displayDetails (Just TsEvent{..}) =
    elAttr "div" ("style" =: "flex-grow: 1; display: flex; flex-direction: column; padding: 7px;"
                  <> "height" =: "100%") $ do
        el "h3" $ text_ desc
        el "h4" $ text_ extraInfo
        case fullContents of
            Nothing   -> return ()
            Just cts  -> elAttr "iframe" ("srcdoc" =: T.unpack cts
                                          <> "frameBorder" =: "0"
                                          <> "width" =: "100%"
                                          <> "style" =: "flex-grow: 1") $ return ()

datePicker :: MonadWidget t m => Day -> m (Event t Day, PikadayPicker)
datePicker initialDay = do
    (e, _) <- elAttr' "div" ("style" =: "width: 250px; position: absolute; z-index: 3") $ return ()
    (evt, evtTrigger) <- newEventWithTriggerRef
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    picker <- liftIO $ do
        cb <- syncCallback1 AlwaysRetain False $ \date ->
            case parsePikadayDate (fromJSString date) of
                Nothing -> return ()
                Just dt -> postGui $ handleTrigger runWithActions dt evtTrigger
        picker <- initPikaday (unwrapElt e) cb
        pickerSetDate picker initialDay
        return picker
    return (evt, picker)

-- the format from pikaday is "Tue Dec 22 2015 00:00:00 GMT+0100 (CET)" for me.
parsePikadayDate :: String -> Maybe Day
parsePikadayDate = parseTimeM False defaultTimeLocale "%a %b %d %Y %X GMT%z (%Z)"
