{-# LANGUAGE DeriveGeneric #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Aeson as JSON
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

instance JSON.ToJSON Event
