{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.TH (mkToJSON, defaultOptions)
import GHC.Generics
import qualified FayAeson

data Event = Event
	{
		pluginName :: String,
		eventIcon :: String,
		eventDate :: UTCTime,
		desc :: T.Text,
		extraInfo :: T.Text,
		fullContents :: Maybe T.Text
	}
	deriving (Eq, Show)

instance ToJSON Event where
     toJSON = FayAeson.addInstance "Event" . $(mkToJSON defaultOptions ''Event)
