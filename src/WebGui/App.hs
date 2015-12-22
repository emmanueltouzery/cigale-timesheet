{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, RecordWildCards #-}

import Reflex
import Reflex.Dom

import GHC.Generics
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Control.Monad

-- url is http://localhost:8000/static/index.html
-- start cigale with .stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/cigale-timesheet

-- TODO unhardcode
initialDay :: String
initialDay = "2015-11-10"

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
    el "div" $ do
        dateInput <- textInput $ def
            & textInputConfig_initialValue .~ initialDay
        let req url = xhrRequest "GET" ("/timesheet/" ++ url) def
        postBuild <- getPostBuild
        let loadRecordsEvent = mergeWith const [textInputGetEnter dateInput, postBuild]
        asyncReq <- performRequestAsync (tag (req <$> current (_textInput_value dateInput)) loadRecordsEvent)
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
