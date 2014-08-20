{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns #-}

module Hg where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )

import Event
import qualified Util
import EventProvider

data HgRecord = HgRecord
	{
		hgUser :: T.Text,
		hgRepo :: FolderPath
	} deriving Show
deriveJSON defaultOptions ''HgRecord

getHgProvider :: EventProvider HgRecord
getHgProvider = EventProvider
	{
		getModuleName = "Hg",
		getEvents = getRepoCommits,
		getConfigType = members $(thGetTypeDesc ''HgRecord)
	}

getRepoCommits :: HgRecord -> GlobalSettings -> Day -> IO [Event.Event]
getRepoCommits (HgRecord _username projectPath) _ day = do
	let username = T.unpack _username
	let dateRange = formatDate day
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "hg" [
			"log", "-k", username, "-d", dateRange,
			"--template", "{date|isodate}\n{desc}\n--->>>\n{files}\n--->>>\n"])
		{
			Process.std_out = Process.CreatePipe,
			Process.cwd = Just projectPath
		}
	output <- IO.hGetContents outh
	timezone <- getTimeZone (UTCTime day 8)
	return $ map (toEvent timezone) $ Util.parsecError parseCommits "Hg.getRepoCommits" output
	
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

parseCommits :: T.GenParser st [Commit]
parseCommits = many parseCommit

parseCommit :: T.GenParser st Commit
parseCommit = do
	date <- parseDateTime
	summary <- parseSummary
	eol
	cFiles <- parseFiles
	return $ Commit date (T.pack summary) cFiles

parseFiles :: T.GenParser st [String]
parseFiles = manyTill parseFile (T.try $ string "--->>>\n")

parseFile :: T.GenParser st String
parseFile = T.many $ T.noneOf " \n" <* T.oneOf " \n"

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	year <- Util.parseNum 4 <* T.char '-'
	month <- Util.parseNum 2 <* T.char '-'
	day <- Util.parseNum 2 <* T.char ' '
	hour <- Util.parseNum 2 <* T.char ':'
	mins <- Util.parseNum 2 <* T.char ' '
	oneOf "-+"
	count 4 digit
	eol
	return $ LocalTime
		(fromGregorian year month day)
		(TimeOfDay hour mins 0)

parseSummary :: T.GenParser st String
parseSummary = manyTill anyChar (T.try $ string "--->>>")

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"
