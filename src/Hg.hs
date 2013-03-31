{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

module Hg (getHgProvider) where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Aeson.TH

import qualified Event
import qualified Util
import EventProvider

data HgRecord = HgRecord
	{
		hgProj :: T.Text,
		hgUser :: T.Text,
		hgRepo :: T.Text
	} deriving Show
deriveJSON id ''HgRecord

getHgProvider :: EventProvider HgRecord
getHgProvider = EventProvider
	{
		getModuleName = "Hg",
		getEvents = getRepoCommits
	}

getRepoCommits :: HgRecord -> Day -> IO [Event.Event]
getRepoCommits (HgRecord project _username _projectPath) day = do
	let username = T.unpack _username
	let projectPath = T.unpack _projectPath
	let dateRange = formatDate day
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "hg" [
			"log", "-k", username, "-d", dateRange,
			"--template", "{date|isodate}\n{desc}\n--->>>\n{files}\n--->>>\n"])
		{
			Process.std_out = Process.CreatePipe,
			Process.cwd = Just projectPath
		}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	timezone <- getCurrentTimeZone
	let parseResult = parseCommitsParsec output
	case parseResult of
		Left pe -> do
			putStrLn $ "HG: parse error: " ++ Util.displayErrors pe
			error "Hg parse error, aborting"
			--return []
		Right x -> return $ map (toEvent project timezone) x
	
toEvent :: T.Text -> TimeZone -> Commit -> Event.Event
toEvent project timezone commit =
	Event.Event (localTimeToUTC timezone (commitDate commit)) 
		(Just $ T.unpack project) (commitDesc commit) (T.pack $ Util.getFilesRoot $ commitFiles commit) Nothing

formatDate :: Day -> String
formatDate day =
	(show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth)
	where
		(year, month, dayOfMonth) = toGregorian day

parseCommitsParsec :: T.Text -> Either ParseError [Commit]
parseCommitsParsec = parse parseCommits ""

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
parseFile = do
	result <- T.many $ T.noneOf " \n"
	T.oneOf " \n"
	return result

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	year <- count 4 digit
	T.char '-'
	month <- count 2 digit
	T.char '-'
	day <- count 2 digit
	T.char ' '
	hour <- count 2 digit
	T.char ':'
	mins <- count 2 digit
	T.char ' '
	oneOf "-+"
	count 4 digit
	eol
	return $ LocalTime
		(fromGregorian (Util.parsedToInteger year) (Util.parsedToInt month) (Util.parsedToInt day))
		(TimeOfDay (Util.parsedToInt hour) (Util.parsedToInt mins) 0)

parseSummary :: T.GenParser st String
parseSummary = manyTill anyChar (T.try $ string "--->>>")

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"
