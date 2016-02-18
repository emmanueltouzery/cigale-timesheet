{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

module Skype where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.List
import System.Directory
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Monad (join)
import Control.Arrow ( (***) )
import Control.Error
import Control.Monad.Trans

import Database.HDBC
import Database.HDBC.Sqlite3

import TsEvent
import qualified Util
import EventProvider

skypeMinIntervalToSplitChatsSeconds :: NominalDiffTime
skypeMinIntervalToSplitChatsSeconds = 3600

data SkypeConfig = SkypeConfig
    {
        skypeUsername :: String
    } deriving Show
deriveJSON defaultOptions ''SkypeConfig

getSkypeProvider :: EventProvider SkypeConfig ()
getSkypeProvider = EventProvider
    {
        getModuleName = "Skype",
        getEvents = getSkypeEvents,
        getConfigType = members $(thGetTypeDesc ''SkypeConfig),
        getExtraData = Nothing
    }

getSkypeEvents :: SkypeConfig -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getSkypeEvents (SkypeConfig skypeUsernameVal) _ day _ = do
    let todayMidnight = LocalTime day (TimeOfDay 0 0 0)
    timezone <- lift $ getTimeZone (UTCTime day 8)
    let todayMidnightUTC = localTimeToUTC timezone todayMidnight
    let minTimestamp = utcTimeToPOSIXSeconds todayMidnightUTC
    let maxTimestamp = minTimestamp + 24*3600
    homeDir <- lift getHomeDirectory
    r <- lift $ do
        conn <- connectSqlite3 $ homeDir ++ "/.Skype/"
            ++ skypeUsernameVal ++ "/main.db"
        result <- quickQuery' conn "select chatname, from_dispname, timestamp, body_xml \
                 \from messages where timestamp >= ? and timestamp <= ? \
                 \and chatname is not null and from_dispname is not null \
                 \and body_xml is not null \
                 \order by timestamp" [SqlPOSIXTime minTimestamp, SqlPOSIXTime maxTimestamp]
        disconnect conn
        return result

    -- get the events grouped by chat
    let eventsAr = fmap messageByChatInfo r
    let eventsMap = Map.fromListWith (flip (++)) eventsAr

    let chatInfos = map snd (Map.toList eventsMap)

    let splitChatInfos = splitFarawayChats chatInfos

    return $ map toEvent splitChatInfos

data ChatRecord = ChatRecord
    {
        messageAuthor :: Text,
        messageTime   :: UTCTime,
        messageText   :: Text
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
splitByCompare notTooFar records = (head records : firstSeries) : splitByCompare notTooFar remains
    where
        (firstSeries, remains) = sndOnly $ span notTooFar $ zip records (tail records)
        sndOnly = join (***) (fmap snd)

-- in reality the list in the second position
-- of the pair will always have one element.
-- it's made like that to easier later call
-- Map.fromListWith
messageByChatInfo :: [SqlValue] -> (String, [ChatRecord])
messageByChatInfo [chatname, author, time, text] = (fromSql chatname,
        [ChatRecord {
            messageAuthor = fromSql author,
            messageTime = posixSecondsToUTCTime $ fromSql time,
            messageText = fromSql text
        }])
messageByChatInfo x@_ = error $ "messageByChatInfo: invalid SQL query results" ++ show x

toEvent :: [ChatRecord] -> TsEvent
toEvent chatRecords = TsEvent
        {
            pluginName = getModuleName getSkypeProvider,
            eventIcon = "glyphicons-245-conversation",
            eventDate = messageTime (head chatRecords),
            desc = T.intercalate ", " $ sort participants,
            extraInfo = extraInfoVal,
            fullContents = Just fullLog
        }
    where
        participants = nub $ map messageAuthor chatRecords
        extraInfoVal = T.pack $ show (length chatRecords) ++ " messages, lasted " ++ durationStr
        durationStr = T.unpack $ Util.formatDurationSec $ diffUTCTime lastTime firstTime
        lastTime = messageTime (last chatRecords)
        firstTime = messageTime (head chatRecords)
        fullLog = T.intercalate "<br/>" (map formatMessage chatRecords)
        formatMessage chatRecord = T.concat ["<b>", messageAuthor chatRecord, ":</b> ", messageText chatRecord]
