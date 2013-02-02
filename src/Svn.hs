{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

module Svn where

import qualified System.Process as Process
import qualified Data.Text.IO as IO
import Data.Time.Calendar
import qualified Data.Text as T
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T

import qualified Util
import qualified Event

import Text.Regex.PCRE.Rex

getRepoCommits :: Day -> Day -> T.Text -> T.Text -> T.Text -> IO [Event.Event]
getRepoCommits startDate endDate username projectName _url = do
	let url = T.unpack _url
	let dateRange = formatDateRange startDate (addDays 1 endDate)
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "svn" ["log", url, "-r", dateRange, "--verbose"])
		{Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	let parseResult = parseCommitsParsec output
	case parseResult of
		Left pe -> do
			putStrLn $ "SVN: parse error: " ++ Util.displayErrors pe
			return []
		Right x -> finishGetRepoCommits x startDate endDate username projectName
	where
		-- TODO this is my best parsec parse error display yet.
		-- share it with hg and ical

finishGetRepoCommits :: [Commit] -> Day -> Day -> T.Text -> T.Text -> IO [Event.Event]
finishGetRepoCommits commits startDate endDate username projectName = do
	let myCommits = filter ((==username) . user) commits
	-- need to filter again by date, because SVN obviously
	-- returns me commits which are CLOSE to the dates I
	-- requested, but not necessarily WITHIN the dates I
	-- requested...
	let myCommitsInInterval = filter ((\d -> d >= startDate && d <= endDate) . localDay . date) myCommits
	timezone <- getCurrentTimeZone
	return $ map (toEvent (T.unpack projectName) timezone) myCommitsInInterval

data Commit = Commit
	{
		date :: LocalTime,
		user :: T.Text,
		comment :: T.Text,
		commitFiles :: [T.Text]
	}
	deriving (Eq, Show)

toEvent :: String -> TimeZone -> Commit -> Event.Event
toEvent projectName timezone (Commit dateVal _ commentVal cFiles) =
	Event.Event (localTimeToUTC timezone dateVal) Event.Svn (Just projectName)
		commentVal (T.pack $ Util.getFilesRoot cFilesStr)
	where
		cFilesStr = map T.unpack cFiles

parseCommitsParsec :: T.Text -> Either ParseError [Commit]
parseCommitsParsec commits = parse parseCommits "" commits

parseCommits :: T.GenParser st [Commit]
parseCommits = do
	parseCommitHeader
	result <- many $ parseCommit
	return result

parseCommit :: T.GenParser st Commit
parseCommit = do
	readCell
	username <- readCell
	dateval <- parseDateTime
	many $ T.noneOf "\r\n"; eol -- finish line
	many $ T.noneOf "\r\n"; eol -- "Changed paths:"
	commitFileInfos <- T.many $ parseCommitFileInfo
	eol
	summary <- parseSummary
	parseCommitHeader
	return $ Commit dateval (T.strip username) (T.pack summary) commitFileInfos

parseCommitHeader :: T.GenParser st T.Text
parseCommitHeader = do
	header <- many $ T.char '-'
	eol
	return $ T.pack header

readCell :: T.GenParser st T.Text
readCell = do
	T.many $ T.char ' '
	result <- T.many $ T.noneOf "|"
	T.char '|'
	return $ T.pack result

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	T.many $ T.char ' '
	year <- count 4 digit
	T.char '-'
	month <- count 2 digit
	T.char '-'
	day <- count 2 digit
	T.char ' '
	hour <- count 2 digit
	T.char ':'
	mins <- count 2 digit
	T.char ':'
	seconds <- count 2 digit
	T.char ' '
	oneOf "-+"
	count 4 digit
	eol
	return $ LocalTime
		(fromGregorian (Util.parsedToInteger year) (Util.parsedToInt month) (Util.parsedToInt day))
		(TimeOfDay (Util.parsedToInt hour) (Util.parsedToInt mins) (fromIntegral $ Util.parsedToInt seconds))

parseCommitFileInfo :: T.GenParser st T.Text
parseCommitFileInfo = do
	count 3 (T.char ' ')
	T.anyChar
	T.char ' '
	filename <- many $ noneOf "\r\n"
	eol
	return $ T.pack filename

parseSummary :: T.GenParser st String 
parseSummary = T.manyTill T.anyChar (T.try $ string "----------")

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day


parseSvnDate :: String -> LocalTime
parseSvnDate [rex|(?{read -> year}\d+)-(?{read -> month}\d+)-
		(?{read -> day}\d+)\s(?{read -> hour}\d+):(?{read -> mins}\d+):
		(?{read -> sec}\d+)|] =
	LocalTime (fromGregorian year month day) (TimeOfDay hour mins sec)
parseSvnDate v@_ = error $ "invalid date " ++ v
