{-# LANGUAGE DeriveGeneric #-}

module Event where

import Data.Time.Clock
import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data Event = Event
    {
        pluginName   :: String,
        eventIcon    :: String,
        eventDate    :: UTCTime,
        desc         :: Text,
        extraInfo    :: Text,
        fullContents :: Maybe Text
    } deriving (Eq, Show, Generic)
instance ToJSON Event
