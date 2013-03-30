{-# LANGUAGE DeriveGeneric #-}

module Event where

import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import GHC.Generics

type Project = String

data Event = Event
	{
		eventDate :: UTCTime,
		project :: Maybe Project,
		desc :: T.Text,
		extraInfo :: T.Text,
		fullContents :: Maybe T.Text
	}
	deriving (Eq, Show, Generic)

instance JSON.ToJSON Event
