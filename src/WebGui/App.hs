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
    "$r = new Pikaday({onSelect: function(picker) { $2(picker.toString()) }}); $1.appendChild($r.el)"
    _initPikaday :: JSRef Element -> JSFun (JSString -> IO ()) -> IO (JSRef a)

initPikaday :: JSRef Element -> JSFun (JSString -> IO ()) -> IO PikadayPicker
initPikaday = fmap (fmap PikadayPicker) . _initPikaday

foreign import javascript unsafe
    "$1.setDate($2, true)" -- the true is to prevent to trigger "onSelect"
    _pickerSetDate :: JSRef a -> JSString -> IO ()

pickerSetDate :: PikadayPicker -> Day -> IO ()
pickerSetDate picker day = _pickerSetDate (unPicker picker) (toJSString $ showGregorian day)

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

cigaleView :: MonadWidget t m => m ()
cigaleView = do
    stylesheet "pikaday.css"
    el "div" $ do
        previousDayBtn <- button "<<"
        rec
            curDate <- foldDyn ($) initialDay $ mergeWith (.)
                [
                    fmap (const $ addDays (-1)) previousDayBtn,
                    fmap (const $ addDays 1) nextDayBtn,
                    --fmap const $ tagDyn (_textInput_value dateInput) pickedDateEvt
                    fmap const pickedDateEvt
                ]
            performEvent_ $ fmap (const $ do
                                  d <- sample (current curDate)
                                  liftIO (pickerSetDate picker d)) $ updated curDate

            dateInput <- textInput $ def
                & textInputConfig_initialValue .~ (showGregorian initialDay)
                & setValue .~ (showGregorian <$> updated curDate)
            nextDayBtn <- button ">>"
            (pickedDateEvt, picker) <- datePicker
        let req url = xhrRequest "GET" ("/timesheet/" ++ url) def
        loadRecordsEvent <- mergeWith const <$> sequence [pure $ updated curDate, fmap (const initialDay) <$> getPostBuild]
        asyncReq <- performRequestAsync (req <$> showGregorian <$> loadRecordsEvent)
        resp <- holdDyn Nothing $ fmap decodeXhrResponse asyncReq
        void (mapDyn eventsTable resp >>= dyn)

-- https://m.reddit.com/r/reflexfrp/comments/3h3s72/rendering_dynamic_html_table/
eventsTable :: MonadWidget t m => Maybe FetchResponse -> m ()
eventsTable Nothing = text "Error reading the server's message!"
eventsTable (Just (FetchResponse events errors)) = el "table" $ mapM_ showRecord events

showRecord :: MonadWidget t m => TsEvent -> m ()
showRecord TsEvent{..} = do
    el "tr" $ do
        el "td" $ text_ desc
        el "td" $ text $ show eventDate

datePicker :: MonadWidget t m => m (Event t Day, PikadayPicker)
datePicker = do
    (e, _) <- elAttr' "div" ("style" =: "width: 250px;") $ return ()
    (evt, evtTrigger) <- newEventWithTriggerRef
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    -- very similar to fireEventRef from Reflex.Host.Class
    -- which I don't have right now.
    -- #reflex-frp on freenode.net, 2015-12-25:
    -- [21:38] <ryantrinkle> the only thing you might want to improve later
    --         is that you could make it so that it subscribes to the event lazily
    -- [21:39] <ryantrinkle> and it unsubscribes when the event gets garbage collected
    -- [21:39] <ryantrinkle> https://hackage.haskell.org/package/reflex-dom-0.2/docs/src/Reflex-Dom-Widget-Basic.html#wrapDomEventMaybe
    let handleTrigger v trigger = liftIO (readIORef trigger) >>= \case
            Nothing       -> return ()
            Just eTrigger -> runWithActions [eTrigger :=> v]
    picker <- liftIO $ do
        cb <- syncCallback1 AlwaysRetain False $ \date ->
            case parsePikadayDate (fromJSString date) of
                Nothing -> return ()
                Just dt -> postGui $ handleTrigger dt evtTrigger
        picker <- initPikaday (unElement $ toElement $ _el_element e) cb
        pickerSetDate picker initialDay
        return picker
    return (evt, picker)

-- the format from pikaday is "Tue Dec 22 2015 00:00:00 GMT+0100 (CET)" for me.
parsePikadayDate :: String -> Maybe Day
parsePikadayDate = parseTimeM False defaultTimeLocale "%a %b %d %Y %X GMT%z (%Z)"

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank
