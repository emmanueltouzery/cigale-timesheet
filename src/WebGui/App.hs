{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface #-}

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.Types hiding (Text, Event)

import Reflex
import Reflex.Dom
import Reflex.Host.Class

import Data.Dependent.Sum (DSum ((:=>)))

import Data.List
import Data.IORef
import GHC.Generics
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Control.Monad
import Data.Time.Calendar
import Data.Time.Format
import Data.Monoid
import Control.Monad.IO.Class

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

-- url is http://localhost:8000/static/index.html
-- start cigale with .stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/cigale-timesheet

-- TODO unhardcode
initialDay :: Day
initialDay = fromGregorian 2015 11 10

text_ :: MonadWidget t m => Text -> m ()
text_ = text . T.unpack

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

main :: IO ()
main = mainWidget cigaleView

performOnChange :: MonadWidget t m => (a -> WidgetHost m ()) -> Dynamic t a -> m ()
performOnChange action dynamic = performEvent_ $
    fmap (const $ sample (current dynamic) >>= action) $ updated dynamic

button' :: MonadWidget t m => String -> m (Event t ())
button' s = do
  (e, _) <- elAttr' "button" ("class" =: "btn btn-secondary btn-sm") $ text s
  return $ domEvent Click e

cigaleView :: MonadWidget t m => m ()
cigaleView = do
    stylesheet "pikaday.css"
    stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.2/css/bootstrap.min.css"
    el "div" $ do
        rec
            -- update current day based on user actions
            curDate <- foldDyn ($) initialDay $ mergeWith (.)
                [
                    fmap (const $ addDays (-1)) previousDayBtn,
                    fmap (const $ addDays 1) nextDayBtn,
                    fmap const pickedDateEvt
                ]
            -- close the datepicker & update its date on day change.
            performOnChange (\d -> liftIO $ do
                dateLabelDeactivate
                pickerSetDate picker d) curDate
            -- open or close the datepicker when the user clicks on the toggle button
            performOnChange
                (\focus -> liftIO $ (if focus then pickerShow else pickerHide) picker)
                cbDyn

            -- the date picker has position: absolute & will position itself
            -- relative to the nearest ancestor with position: relative.
            (previousDayBtn, dateLabelDeactivate, nextDayBtn, cbDyn) <-
                elAttr "div" ("class" =: "btn-group" <> "data-toggle" =: "buttons" <> "style" =: "position: relative") $ do
                    pDayBtn <- button' "<<"
                    (cbDn, lbelDeac) <- createDateLabel curDate
                    nDayBtn <- button' ">>"
                    return (pDayBtn, lbelDeac, nDayBtn, cbDn)
            (pickedDateEvt, picker) <- datePicker
        liftIO $ pickerHide picker
        let req url = xhrRequest "GET" ("/timesheet/" ++ url) def
        loadRecordsEvent <- mergeWith const <$> sequence [pure $ updated curDate, fmap (const initialDay) <$> getPostBuild]
        asyncReq <- performRequestAsync (req <$> showGregorian <$> loadRecordsEvent)
        resp <- holdDyn Nothing $ fmap decodeXhrResponse asyncReq
        void (mapDyn eventsTable resp >>= dyn)

createDateLabel :: MonadWidget t m => Dynamic t Day -> m (Dynamic t Bool, IO ())
createDateLabel curDate = do
    -- the bootstrap toggle button doesn't update the underlying checkbox (!!!)
    -- => must catch the click event myself & look at the active class of the label...
    -- I create a new event because I need IO to check the class of the element
    (dayToggleEvt, dayToggleEvtTrigger) <- newEventWithTriggerRef
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    cbDyn <- holdDyn False dayToggleEvt

    (label, _) <- elAttr' "label" ("class" =: "btn btn-secondary btn-sm" <> "style" =: "margin-bottom: 0px") $ do
        -- TODO i would expect the checkbox to check or uncheck propertly but it doesn't.
        -- it's completely user-invisible though.
        void $ checkbox False $ def
            & setValue .~ dayToggleEvt
        dynText =<< mapDyn (formatTime defaultTimeLocale "%A, %F") curDate

    performEvent_ $ fmap (const $ liftIO $ do
        (cn :: String) <- elementGetClassName (_el_element label)
        postGui $ handleTrigger runWithActions ("active" `isInfixOf` cn) dayToggleEvtTrigger) $ domEvent Click label

    let dateLabelDeactivate = do
        eltStripClass (_el_element label) "active"
        postGui (handleTrigger runWithActions False dayToggleEvtTrigger)
    return (cbDyn, dateLabelDeactivate)

eltStripClass :: IsElement self => self -> Text -> IO ()
eltStripClass elt className = do
    curClasses <- T.splitOn " " <$> T.pack <$> elementGetClassName elt
    let newClasses = T.unpack <$> filter (/= className) curClasses
    elementSetClassName elt (intercalate " " newClasses)

-- https://m.reddit.com/r/reflexfrp/comments/3h3s72/rendering_dynamic_html_table/
eventsTable :: MonadWidget t m => Maybe FetchResponse -> m (Dynamic t (Maybe TsEvent))
eventsTable Nothing = text "Error reading the server's message!" >> return (constDyn Nothing)
eventsTable (Just (FetchResponse tsEvents errors)) = elAttr "table" ("class" =: "table") $ do
    rec
        events <- mapM (showRecord curEventDyn) tsEvents
        curEventDyn <- holdDyn Nothing $ mergeWith const $ fmap (fmap Just) events
    return curEventDyn

showRecord :: MonadWidget t m => Dynamic t (Maybe TsEvent) -> TsEvent -> m (Event t TsEvent)
showRecord curEventDyn tsEvt@TsEvent{..} = do
    rowAttrs <- mapDyn (\curEvt -> "class" =: if curEvt == Just tsEvt then "table-active" else "") curEventDyn
    (e, _) <- elDynAttr' "tr" rowAttrs $ do
        el "td" $ text $ formatTime defaultTimeLocale "%R" eventDate
        el "td" $ text_ desc
    return (const tsEvt <$> domEvent Click e)

datePicker :: MonadWidget t m => m (Event t Day, PikadayPicker)
datePicker = do
    (e, _) <- elAttr' "div" ("style" =: "width: 250px; position: absolute; z-index: 3") $ return ()
    (evt, evtTrigger) <- newEventWithTriggerRef
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    picker <- liftIO $ do
        cb <- syncCallback1 AlwaysRetain False $ \date ->
            case parsePikadayDate (fromJSString date) of
                Nothing -> return ()
                Just dt -> postGui $ handleTrigger runWithActions dt evtTrigger
        picker <- initPikaday (unElement $ toElement $ _el_element e) cb
        pickerSetDate picker initialDay
        return picker
    return (evt, picker)

-- very similar to fireEventRef from Reflex.Host.Class
-- which I don't have right now.
-- #reflex-frp on freenode.net, 2015-12-25:
-- [21:38] <ryantrinkle> the only thing you might want to improve later
--         is that you could make it so that it subscribes to the event lazily
-- [21:39] <ryantrinkle> and it unsubscribes when the event gets garbage collected
-- [21:39] <ryantrinkle> https://hackage.haskell.org/package/reflex-dom-0.2/docs/src/Reflex-Dom-Widget-Basic.html#wrapDomEventMaybe
handleTrigger :: MonadIO m => ([DSum tag] -> m ()) -> a -> IORef (Maybe (tag a)) -> m ()
handleTrigger runWithActions v trigger = liftIO (readIORef trigger) >>= \case
        Nothing       -> return ()
        Just eTrigger -> runWithActions [eTrigger :=> v]

-- the format from pikaday is "Tue Dec 22 2015 00:00:00 GMT+0100 (CET)" for me.
parsePikadayDate :: String -> Maybe Day
parsePikadayDate = parseTimeM False defaultTimeLocale "%a %b %d %Y %X GMT%z (%Z)"

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank
