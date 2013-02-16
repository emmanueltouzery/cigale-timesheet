{-# LANGUAGE OverloadedStrings #-}
module Skype (getSkypeEvents) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List
import System.Directory

import Database.HDBC
import Database.HDBC.Sqlite3

import Event
import qualified Util

getSkypeEvents :: Day -> String -> IO [Event]
getSkypeEvents day skypeUsername = do
	let todayMidnight = LocalTime day (TimeOfDay 0 0 0)
	timezone <- getCurrentTimeZone
	let todayMidnightUTC = localTimeToUTC timezone todayMidnight
	let minTimestamp = utcTimeToPOSIXSeconds todayMidnightUTC
	let maxTimestamp = minTimestamp + 24*3600
	homeDir <- getHomeDirectory
	conn <- connectSqlite3 $ homeDir ++ "/.Skype/" ++ skypeUsername ++ "/main.db"
	r <- quickQuery' conn "select chatname, from_dispname, timestamp, body_xml \
				 \from messages where timestamp >= ? and timestamp <= ? \
				 \and chatname is not null and from_dispname is not null \
				 \and body_xml is not null \
				 \order by timestamp" [SqlPOSIXTime minTimestamp, SqlPOSIXTime maxTimestamp]
	disconnect conn

	-- get the events grouped by chat
	let eventsAr = fmap messageByChatInfo r
	let eventsMap = Map.fromListWith (++) eventsAr

	return $ map toEvent $ Map.toList eventsMap

data ChatRecord = ChatRecord
	{
		messageAuthor :: T.Text,
		messageTime :: UTCTime,
		messageText :: T.Text
	} deriving (Eq, Show)

-- in reality the list in the second position
-- of the pair will always have one element.
-- it's made like that to easier later call 
-- Map.fromListWith
messageByChatInfo :: [SqlValue] -> (String, [ChatRecord])
messageByChatInfo dbRow = (fromSql $ dbRow !! 0,
		[ChatRecord {
			messageAuthor = fromSql $ dbRow !! 1,
			messageTime = posixSecondsToUTCTime $ fromSql $ dbRow !! 2,
			messageText = fromSql $ dbRow !! 3
		}])

toEvent :: (String, [ChatRecord]) -> Event
toEvent chat = Event
		{
			eventDate = messageTime (head chatRecords),
			eventType = Event.Chat,
			project = Nothing,
			desc = participantsStr,
			extraInfo = extraInfoVal
			-- full log
		}
	where
		chatRecords = snd chat
		participantsStr = T.intercalate ", " $ sort participants
		participants = nub $ map messageAuthor chatRecords
		extraInfoVal = T.pack $ (show $ length chatRecords) ++ " messages, lasted " ++ durationStr
		durationStr = T.unpack $ Util.formatDurationSec $ diffUTCTime firstTime lastTime
		lastTime = messageTime (last $ chatRecords)
		firstTime = messageTime (head $ chatRecords)
