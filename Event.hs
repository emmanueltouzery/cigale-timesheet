{-# LANGUAGE DeriveGeneric #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Aeson as JSON
import GHC.Generics
import Data.Text.Read

data EventType = Svn
	| Email
	| Calendar
	deriving (Eq, Show, Generic)

instance JSON.ToJSON EventType

type Project = String

data Event = Event
	{
		eventDate :: UTCTime,
		eventType :: EventType,
		project :: Maybe Project,
		extraInfo :: T.Text
	}
	deriving (Eq, Show, Generic)

instance JSON.ToJSON Event
