{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns #-}

module Hg where

import Data.Time.Calendar
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Text.Parsec.Text
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Monad.Trans
import Control.Error

import TsEvent
import Util
import EventProvider
import EventProviderSettings

deriveConfigRecord hgConfigDataType
deriveJSON defaultOptions ''HgConfigRecord

getHgProvider :: EventProvider HgConfigRecord ()
getHgProvider = EventProvider
    {
        getModuleName = "Hg",
        getEvents     = getRepoCommits,
        getConfigType = members hgConfigDataType,
        getExtraData  = Nothing
    }

getRepoCommits :: HgConfigRecord -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getRepoCommits (HgConfigRecord username projectPath) _ day _ = do
    output <- Util.runProcess "hg" projectPath
        [
            "log", "-k", T.unpack username, "-d", showGregorian day,
            "--template", "{date|isodate}\n{desc}\n--->>>\n{files}\n--->>>\n"
        ]
    timezone <- liftIO $ getTimeZone (UTCTime day 8)
    commits <- hoistEither $ fmapL show $ parse parseCommits "" output
    return $ map (toEvent timezone) commits

toEvent :: TimeZone -> Commit -> TsEvent
toEvent timezone commit = TsEvent
    {
        pluginName = getModuleName getHgProvider,
        eventIcon = "glyphicons-423-git-branch",
        eventDate = localTimeToUTC timezone (commitDate commit),
        desc = commitDesc commit,
        extraInfo = T.pack $ Util.getFilesRoot $ commitFiles commit,
        fullContents = Nothing
    }

data Commit = Commit
    {
        commitDate  :: LocalTime,
        commitDesc  :: Text,
        commitFiles :: [String]
    }
    deriving (Eq, Show)

parseCommits :: GenParser st [Commit]
parseCommits = many parseCommit

parseCommit :: GenParser st Commit
parseCommit = do
    date <- parseDateTime
    summary <- parseSummary
    eol
    cFiles <- parseFiles
    return $ Commit date (T.pack summary) cFiles

parseFiles :: GenParser st [String]
parseFiles = manyTill parseFile (try $ string "--->>>\n")

parseFile :: GenParser st String
parseFile = do
    result <- many $ noneOf " \n"
    oneOf " \n"
    return result

parseDateTime :: GenParser st LocalTime
parseDateTime = do
    year  <- Util.parseNum 4 <* char '-'
    month <- Util.parseNum 2 <* char '-'
    day   <- Util.parseNum 2 <* char ' '
    hour  <- Util.parseNum 2 <* char ':'
    mins  <- Util.parseNum 2 <* char ' '
    oneOf "-+"
    count 4 digit
    eol
    return $ LocalTime
        (fromGregorian year month day)
        (TimeOfDay hour mins 0)

parseSummary :: GenParser st String
parseSummary = manyTill anyChar (try $ string "--->>>")
