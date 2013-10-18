{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

module Skype where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List
import System.Directory
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Database.HDBC
import Database.HDBC.Sqlite3

import Event
import qualified Util
import EventProvider

skypeMinIntervalToSplitChatsSeconds :: NominalDiffTime
skypeMinIntervalToSplitChatsSeconds = 3600

data SkypeConfig = SkypeConfig
	{
		skypeUsername :: String
	} deriving Show
deriveJSON defaultOptions ''SkypeConfig

getSkypeProvider :: EventProvider SkypeConfig
getSkypeProvider = EventProvider
	{
		getModuleName = "Skype",
		getEvents = getSkypeEvents,
		getConfigType = members $(thGetTypeDesc ''SkypeConfig)
	}

getSkypeEvents :: SkypeConfig -> GlobalSettings -> Day -> IO [Event]
getSkypeEvents (SkypeConfig skypeUsernameVal) _ day = do
	let todayMidnight = LocalTime day (TimeOfDay 0 0 0)
	timezone <- getCurrentTimeZone
	let todayMidnightUTC = localTimeToUTC timezone todayMidnight
	let minTimestamp = utcTimeToPOSIXSeconds todayMidnightUTC
	let maxTimestamp = minTimestamp + 24*3600
	homeDir <- getHomeDirectory
	conn <- connectSqlite3 $ homeDir ++ "/.Skype/" 
		++ skypeUsernameVal ++ "/main.db"
	r <- quickQuery' conn "select chatname, from_dispname, timestamp, body_xml \
				 \from messages where timestamp >= ? and timestamp <= ? \
				 \and chatname is not null and from_dispname is not null \
				 \and body_xml is not null \
				 \order by timestamp" [SqlPOSIXTime minTimestamp, SqlPOSIXTime maxTimestamp]
	disconnect conn

	-- get the events grouped by chat
	let eventsAr = fmap messageByChatInfo r
	let eventsMap = Map.fromListWith (flip (++)) eventsAr

	let chatInfos = map snd (Map.toList eventsMap)

	let splitChatInfos = splitFarawayChats chatInfos

	return $ map toEvent splitChatInfos

data ChatRecord = ChatRecord
	{
		messageAuthor :: T.Text,
		messageTime :: UTCTime,
		messageText :: T.Text
	} deriving (Eq, Show)

splitFarawayChats :: [[ChatRecord]] -> [[ChatRecord]]
splitFarawayChats = concatMap splitChat

splitChat :: [ChatRecord] -> [[ChatRecord]]
splitChat = splitByCompare notTooFar
	where
		notTooFar (a,b) = diffUTCTime (messageTime b) (messageTime a) < skypeMinIntervalToSplitChatsSeconds

-- TODO maybe unfoldr would make sense here...
splitByCompare :: ((a,a)->Bool) -> [a] -> [[a]]
splitByCompare _ [] = []
splitByCompare notTooFar records = ((head records : firstSeries) : splitByCompare notTooFar remains)
	where
		(firstSeries, remains) = sndOnly $ span notTooFar $ zip records (tail records)
		sndOnly (a,b) = (fmap snd a, fmap snd b)

-- in reality the list in the second position
-- of the pair will always have one element.
-- it's made like that to easier later call 
-- Map.fromListWith
messageByChatInfo :: [SqlValue] -> (String, [ChatRecord])
messageByChatInfo dbRow = (fromSql $ head dbRow,
		[ChatRecord {
			messageAuthor = fromSql $ dbRow !! 1,
			messageTime = posixSecondsToUTCTime $ fromSql $ dbRow !! 2,
			messageText = fromSql $ dbRow !! 3
		}])

toEvent :: [ChatRecord] -> Event
toEvent chatRecords = Event
		{
			pluginName = getModuleName getSkypeProvider,
			eventDate = messageTime (head chatRecords),
			desc = participantsStr,
			extraInfo = extraInfoVal,
			fullContents = Just fullLogEscaped
		}
	where
		participantsStr = T.intercalate ", " $ sort participants
		participants = nub $ map messageAuthor chatRecords
		extraInfoVal = T.pack $ (show $ length chatRecords) ++ " messages, lasted " ++ durationStr
		durationStr = T.unpack $ Util.formatDurationSec $ diffUTCTime lastTime firstTime
		lastTime = messageTime (last chatRecords)
		firstTime = messageTime (head chatRecords)
		fullLogEscaped = T.replace "\"" "&quot;" fullLog
		fullLog = T.intercalate "<br/>" (map formatMessage chatRecords)
		formatMessage chatRecord = T.concat ["<b>", messageAuthor chatRecord, ":</b> ", messageText chatRecord]
