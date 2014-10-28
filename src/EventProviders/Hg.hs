{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns #-}

module Hg where

import Data.Time.Calendar
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Text.Parsec.Text
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad.Trans
import Control.Error

import Event
import qualified Util
import EventProvider

data HgRecord = HgRecord
	{
		hgUser :: T.Text,
		hgRepo :: FolderPath
	} deriving Show
deriveJSON defaultOptions ''HgRecord

getHgProvider :: EventProvider HgRecord ()
getHgProvider = EventProvider
	{
		getModuleName = "Hg",
		getEvents = getRepoCommits,
		getConfigType = members $(thGetTypeDesc ''HgRecord),
		getExtraData = Nothing
	}

getRepoCommits :: HgRecord -> GlobalSettings -> Day -> (() -> Url) -> EitherT String IO [Event.Event]
getRepoCommits (HgRecord _username projectPath) _ day _ = do
	let username = T.unpack _username
	let dateRange = formatDate day
	output <- Util.runProcess "hg" projectPath ["log", "-k", username, "-d", dateRange,
			"--template", "{date|isodate}\n{desc}\n--->>>\n{files}\n--->>>\n"]
	timezone <- liftIO $ getTimeZone (UTCTime day 8)
	commits <- hoistEither $ fmapL show $ parse parseCommits "" output
	return $ map (toEvent timezone) commits
	
toEvent :: TimeZone -> Commit -> Event.Event
toEvent timezone commit =
	Event.Event
		{
			pluginName = getModuleName getHgProvider,
			eventIcon = "glyphicon-cog",
			eventDate = localTimeToUTC timezone (commitDate commit),
			desc = commitDesc commit,
			extraInfo = T.pack $ Util.getFilesRoot $ commitFiles commit,
			fullContents = Nothing
		}

formatDate :: Day -> String
formatDate (toGregorian -> (year, month, dayOfMonth)) =
	show year ++ "-" ++ show month ++ "-" ++ show dayOfMonth

data Commit = Commit
	{
		commitDate :: LocalTime,
		commitDesc :: T.Text,
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
parseFile = manyTill anyChar $ string " \n"

parseDateTime :: GenParser st LocalTime
parseDateTime = do
	year <- Util.parseNum 4 <* char '-'
	month <- Util.parseNum 2 <* char '-'
	day <- Util.parseNum 2 <* char ' '
	hour <- Util.parseNum 2 <* char ':'
	mins <- Util.parseNum 2 <* char ' '
	oneOf "-+"
	count 4 digit
	eol
	return $ LocalTime
		(fromGregorian year month day)
		(TimeOfDay hour mins 0)

parseSummary :: GenParser st String
parseSummary = manyTill anyChar (try $ string "--->>>")

eol :: GenParser st String
eol = many $ oneOf "\r\n"
