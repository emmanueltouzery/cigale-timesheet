{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson as JSON
import GHC.Generics

data Event = Event
	{
		pluginName :: String,
		eventDate :: UTCTime,
		desc :: T.Text,
		extraInfo :: T.Text,
		fullContents :: Maybe T.Text
	}
	deriving (Eq, Show, Generic)

--instance JSON.ToJSON Event
instance ToJSON Event where
     toJSON (Event name date desc extra full) = object [
	"instance" .= T.pack "Event",
	"pluginName" .= name,
	"eventDate" .= date,
	"desc" .= desc,
	"extraInfo" .= extra,
	"fullContents" .= full]
