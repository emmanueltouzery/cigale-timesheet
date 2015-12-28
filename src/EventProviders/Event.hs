module Event where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as T

data Event = Event
    {
        pluginName :: String,
        eventIcon :: String,
        eventDate :: UTCTime,
        desc :: T.Text,
        extraInfo :: T.Text,
        fullContents :: Maybe T.Text
    } deriving (Eq, Show)
