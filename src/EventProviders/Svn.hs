{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell #-}

module Svn where

import qualified System.Process as Process
import qualified Data.Text.IO as IO
import Data.Time.Calendar
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )

import qualified Util
import Event
import EventProvider

import Text.Regex.PCRE.Rex

data SvnConfigRecord = SvnConfigRecord
	{
		svnUser :: String,
		svnRepo :: String
	} deriving Show
deriveJSON defaultOptions ''SvnConfigRecord

getSvnProvider :: EventProvider SvnConfigRecord
getSvnProvider = EventProvider
	{
		getModuleName = "Svn",
		getEvents = getRepoCommits,
		getConfigType = members $(thGetTypeDesc ''SvnConfigRecord)
	}

getRepoCommits :: SvnConfigRecord -> GlobalSettings -> Day -> IO [Event.Event]
getRepoCommits config _ date = do
	let dateRange = formatDateRange date (addDays 1 date)
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "svn" ["log", svnRepo config, 
				"-r", dateRange, "--verbose"])
		{Process.std_out = Process.CreatePipe}
	output <- IO.hGetContents outh
	let parseResult = parseCommitsParsec output
	case parseResult of
		Left pe -> do
			putStrLn $ "SVN: parse error: " ++ Util.displayErrors pe
			error "Svn parse error, aborting"
			--return []
		Right x -> finishGetRepoCommits x date date
			(T.pack $ svnUser config)

finishGetRepoCommits :: [Commit] -> Day -> Day -> T.Text -> IO [Event.Event]
finishGetRepoCommits commits startDate endDate username = do
	let myCommits = filter ((==username) . user) commits
	-- need to filter again by date, because SVN obviously
	-- returns me commits which are CLOSE to the dates I
	-- requested, but not necessarily WITHIN the dates I
	-- requested...
	let myCommitsInInterval = filter
		((\d -> d >= startDate && d <= endDate) . localDay . date)
		myCommits
	timezone <- getTimeZone (UTCTime startDate 8)
	return $ map (toEvent timezone) myCommitsInInterval

data Commit = Commit
	{
		date :: LocalTime,
		user :: T.Text,
		comment :: T.Text,
		commitFiles :: [T.Text]
	}
	deriving (Eq, Show)

toEvent :: TimeZone -> Commit -> Event.Event
toEvent timezone (Commit dateVal _ commentVal cFiles) =
	Event.Event
		{
			pluginName = getModuleName getSvnProvider,
			eventIcon = "glyphicon-cog",
			eventDate = localTimeToUTC timezone dateVal,
			desc = commentVal,
			extraInfo = T.pack $ Util.getFilesRoot cFilesStr,
			fullContents = Nothing
		}
	where
		cFilesStr = map T.unpack cFiles

parseCommitsParsec :: T.Text -> Either ParseError [Commit]
parseCommitsParsec = parse parseCommits ""

parseCommits :: T.GenParser st [Commit]
parseCommits = parseCommitHeader >> many parseCommit

parseCommit :: T.GenParser st Commit
parseCommit = do
	readCell
	username <- readCell
	dateval <- parseDateTime
	many $ T.noneOf "\r\n"; eol -- finish line
	many $ T.noneOf "\r\n"; eol -- "Changed paths:"
	commitFileInfos <- T.many parseCommitFileInfo
	eol
	summary <- parseSummary
	parseCommitHeader
	return $ Commit dateval (T.strip username) (T.pack summary) commitFileInfos

parseCommitHeader :: T.GenParser st T.Text
parseCommitHeader = T.pack <$> many (T.char '-') <* eol

readCell :: T.GenParser st T.Text
readCell = do
	T.many $ T.char ' '
	result <- T.many $ T.noneOf "|"
	T.char '|'
	return $ T.pack result

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	T.many $ T.char ' '
	year <- Util.parseNum 4 <* T.char '-'
	month <- Util.parseNum 2 <* T.char '-'
	day <- Util.parseNum 2 <* T.char ' '
	hour <- Util.parseNum 2 <* T.char ':'
	mins <- Util.parseNum 2 <* T.char ':'
	seconds <- Util.parseNum 2 <* T.char ' '
	oneOf "-+"
	count 4 digit
	eol
	return $ LocalTime
		(fromGregorian year month day)
		(TimeOfDay hour mins seconds)

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
	"{" ++ show year ++ "-" ++ show month ++ "-" ++ show dayOfMonth ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day


parseSvnDate :: String -> LocalTime
parseSvnDate [rex|(?{read -> year}\d+)-(?{read -> month}\d+)-
		(?{read -> day}\d+)\s(?{read -> hour}\d+):(?{read -> mins}\d+):
		(?{read -> sec}\d+)|] =
	LocalTime (fromGregorian year month day) (TimeOfDay hour mins sec)
parseSvnDate v@_ = error $ "invalid date " ++ v
