{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell #-}

module Svn where

import Data.Time.Calendar
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Text.Parsec.Text
import Text.Parsec
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ( (<*>), (<*), (*>) )
import Control.Monad.Trans
import Control.Error

import qualified Util
import TsEvent
import EventProvider

data SvnConfigRecord = SvnConfigRecord
    {
        svnUser :: String,
        svnRepo :: String
    } deriving Show
deriveJSON defaultOptions ''SvnConfigRecord

getSvnProvider :: EventProvider SvnConfigRecord ()
getSvnProvider = EventProvider
    {
        getModuleName = "Svn",
        getEvents = getRepoCommits,
        getConfigType = members $(thGetTypeDesc ''SvnConfigRecord),
        getExtraData = Nothing
    }

getRepoCommits :: SvnConfigRecord -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getRepoCommits config _ day _ = do
    let dateRange = formatDateRange day (addDays 1 day)
    output <- Util.runProcess "svn" "."
        ["log", svnRepo config, "-r", dateRange, "--verbose"]
    commits <- hoistEither $ fmapL show $ parse parseCommits "" output
    lift $ finishGetRepoCommits day day (T.pack $ svnUser config) commits

finishGetRepoCommits :: Day -> Day -> Text -> [Commit] -> IO [TsEvent]
finishGetRepoCommits startDate endDate username commits = do
    let myCommits = filter ((==username) . user) commits
    -- need to filter again by date, because SVN obviously
    -- returns me commits which are CLOSE to the dates I
    -- requested, but not necessarily WITHIN the dates I
    -- requested...
    let myCommitsInInterval =
            filter ((\d -> d >= startDate && d <= endDate) . localDay . date) myCommits
    timezone <- getTimeZone (UTCTime startDate 8)
    return $ map (toEvent timezone) myCommitsInInterval

data Commit = Commit
    {
        date :: LocalTime,
        user :: Text,
        comment :: Text,
        commitFiles :: [Text]
    }
    deriving (Eq, Show)

toEvent :: TimeZone -> Commit -> TsEvent
toEvent timezone (Commit dateVal _ commentVal cFiles) = TsEvent
    {
        pluginName = getModuleName getSvnProvider,
        eventIcon = "glyphicons-423-git-branch",
        eventDate = localTimeToUTC timezone dateVal,
        desc = commentVal,
        extraInfo = T.pack $ Util.getFilesRoot cFilesStr,
        fullContents = Nothing
    }
    where
        cFilesStr = map T.unpack cFiles

parseCommits :: GenParser st [Commit]
parseCommits = parseCommitHeader >> many parseCommit

parseCommit :: GenParser st Commit
parseCommit = do
    readCell
    username <- readCell
    dateval <- parseDateTime
    many $ noneOf "\r\n"; eol -- finish line
    many $ noneOf "\r\n"; eol -- "Changed paths:"
    commitFileInfos <- many parseCommitFileInfo
    eol
    summary <- parseSummary
    parseCommitHeader
    return $ Commit dateval (T.strip username) (T.pack summary) commitFileInfos

parseCommitHeader :: GenParser st Text
parseCommitHeader = T.pack <$> many (char '-') <* eol

readCell :: GenParser st Text
readCell = do
    many $ char ' '
    result <- many $ noneOf "|"
    char '|'
    return $ T.pack result

parseDateTime :: GenParser st LocalTime
parseDateTime = do
    many $ char ' '
    year  <- Util.parseNum 4 <* char '-'
    month <- Util.parseNum 2 <* char '-'
    day   <- Util.parseNum 2 <* char ' '
    hour  <- Util.parseNum 2 <* char ':'
    mins  <- Util.parseNum 2 <* char ':'
    seconds <- Util.parseNum 2 <* char ' '
    oneOf "-+"
    count 4 digit
    eol
    return $ LocalTime
        (fromGregorian year month day)
        (TimeOfDay hour mins seconds)

parseCommitFileInfo :: GenParser st Text
parseCommitFileInfo = do
    count 3 (char ' ')
    anyChar
    char ' '
    filename <- many $ noneOf "\r\n"
    eol
    return $ T.pack filename

parseSummary :: GenParser st String
parseSummary = manyTill anyChar (try $ string "----------")

eol :: GenParser st String
eol = many $ oneOf "\r\n"

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
    formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate (toGregorian -> (year, month, dayOfMonth)) =
    "{" ++ show year ++ "-" ++ show month ++ "-" ++ show dayOfMonth ++ "}"
