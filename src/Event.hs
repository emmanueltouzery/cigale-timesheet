{-# LANGUAGE DeriveGeneric #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import GHC.Generics

data EventType = Svn
	| Email
	| Calendar
	| Chat
	deriving (Eq, Show, Generic)

instance JSON.ToJSON EventType

type Project = String

data Event = Event
	{
		eventDate :: UTCTime,
		eventType :: EventType,
		project :: Maybe Project,
		desc :: T.Text,
		extraInfo :: T.Text,
		fullContents :: Maybe T.Text
	}
	deriving (Eq, Show, Generic)

instance JSON.ToJSON Event
