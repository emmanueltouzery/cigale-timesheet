{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase #-}

import GHCJS.DOM.Element
import Reflex
import Reflex.Dom

import GHC.Generics
import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson
import Control.Monad
import Control.Monad.IO.Class

-- 2015-11-10 is a good value
-- url is http://localhost:8000/static/index.html
-- start cigale with .stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/cigale-timesheet

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
        dateInput <- textInput def
        let req url = xhrRequest "GET" ("/timesheet/" ++ url) def
        eventsTable
        asyncReq <- performRequestAsync (tag (req <$> current (_textInput_value dateInput)) $ textInputGetEnter dateInput)
        resp <- holdDyn Nothing $ fmap decodeXhrResponse asyncReq
        --liftIO $ putStrLn "hello"
        dynText =<< mapDyn (\case
            Nothing     -> "Error reading the server's message!"
            Just r -> show $ desc $ head $ fetchedEvents r) resp
        return ()

-- https://m.reddit.com/r/reflexfrp/comments/3h3s72/rendering_dynamic_html_table/
eventsTable :: MonadWidget t m => m ()
eventsTable = el "table" $ do
    el "tr" $ do
        el "td" $ text "test"
        el "td" $ text "test1"
