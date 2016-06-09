{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, RecordWildCards #-}

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
import Control.Monad
import Control.Arrow ( (***) )
import Control.Error
import Control.Monad.Trans
import System.FilePath.Posix
import Control.Exception
import System.IO.Error
import Data.Monoid

import Database.HDBC
import Database.HDBC.Sqlite3

import TsEvent
import qualified Util
import EventProvider
import EventProviderSettings

skypeMinIntervalToSplitChatsSeconds :: NominalDiffTime
skypeMinIntervalToSplitChatsSeconds = 3600

mainDb :: FilePath
mainDb = "main.db"

deriveConfigRecord skypeConfigDataType
deriveJSON defaultOptions ''SkypeConfigRecord

getSkypeProvider :: EventProvider SkypeConfigRecord ()
getSkypeProvider = EventProvider
    {
        getModuleName = "Skype",
        getEvents     = getSkypeEvents,
        getConfigType = members skypeConfigDataType,
        getExtraData  = Nothing,
        fetchFieldCts = Just fetchFieldContents
    }

fetchFieldContents :: ConfigDataInfo -> Maybe SkypeConfigRecord -> GlobalSettings
                   -> ExceptT String IO [Text]
fetchFieldContents cfgDataItem mCfg _
    | cfgDataItem == cfgItemSkypeConversationsHide =
          case mCfg of
            Nothing  -> return []
            Just cfg -> fetchConversations cfg
    | cfgDataItem == cfgItemSkypeUsername      = fetchUsernameContents
    | otherwise = error ("wrong data item " <> show cfgDataItem)

fetchUsernameContents :: ExceptT String IO [Text]
fetchUsernameContents = fmap T.pack <$> getSkypeUsers

fetchConversations :: SkypeConfigRecord -> ExceptT String IO [Text]
fetchConversations cfg = fmap (fromSql . head) <$>
    lift (quickSkypeQuery' cfg "select distinct displayname from conversations order by lower(displayname)" [])

getSkypeUsers :: ExceptT String IO [String]
getSkypeUsers = lift $ catchJust (guard . isDoesNotExistError) getUsersInternal (const $ return [])
    where getUsersInternal = do
              skypeDir   <- skypeBaseDir
              candidates <- sort . filter (not . isPrefixOf ".") <$>
                            getDirectoryContents skypeDir
              filterM (\d -> doesFileExist $ skypeDir </> d </> mainDb) candidates

skypeBaseDir :: IO FilePath
skypeBaseDir = (</> ".Skype/") <$> getHomeDirectory

withSkypeConn :: SkypeConfigRecord -> (Connection -> IO a) -> IO a
withSkypeConn SkypeConfigRecord{..} f = do
    skypeDir <- skypeBaseDir
    conn <- connectSqlite3 $ skypeDir
            ++ T.unpack skypeUsername ++ "/" ++ mainDb
    r <- f conn
    disconnect conn
    return r

quickSkypeQuery' :: SkypeConfigRecord -> String -> [SqlValue] -> IO [[SqlValue]]
quickSkypeQuery' skypeCfg sql params =
    withSkypeConn skypeCfg $ \conn -> quickQuery' conn sql params

getSkypeEvents :: SkypeConfigRecord -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getSkypeEvents skypeCfg _ day _ = do
    let todayMidnight = LocalTime day (TimeOfDay 0 0 0)
    timezone <- lift $ getTimeZone (UTCTime day 8)
    let todayMidnightUTC = localTimeToUTC timezone todayMidnight
    let minTimestamp = utcTimeToPOSIXSeconds todayMidnightUTC
    let maxTimestamp = minTimestamp + 24*3600
    let convsToHide = skypeConversationsHide skypeCfg
    let convsToHideParams = intersperse ',' $ const '?' <$> convsToHide
    let allParams = [SqlPOSIXTime minTimestamp, SqlPOSIXTime maxTimestamp] ++
                    (SqlString . T.unpack <$> convsToHide)
    r <- lift $ quickSkypeQuery' skypeCfg
         ("select chatname, from_dispname, timestamp, body_xml \
         \from messages \
         \join conversations on messages.convo_id = conversations.id \
         \where timestamp >= ? and timestamp <= ? \
         \and chatname is not null and from_dispname is not null \
         \and body_xml is not null \
         \and not conversations.displayname in (" ++ convsToHideParams ++ ") \
         \order by timestamp") allParams

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
        formatMessage chatRecord = Util.linksForceNewWindow $
            T.concat ["<b>", messageAuthor chatRecord, ":</b> ", messageText chatRecord]
