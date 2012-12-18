module Event where

import Data.Time.Clock
import qualified Data.Text as T

data EventType = Svn
	| Email

type Project = String

data Event = Event
	{
		eventDate :: UTCTime,
		eventType :: EventType,
		project :: Project,
		extraInfo :: T.Text
	}
