{-# LANGUAGE OverloadedStrings #-}

module Git where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.List (isInfixOf, intercalate)

import qualified Event
import qualified Util

getRepoCommits :: Day -> T.Text -> T.Text -> T.Text -> IO [Event.Event]
getRepoCommits startDate _username project _projectPath = do
	let username = T.unpack _username
	let projectPath = T.unpack _projectPath
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "git" [
			"log", "--since", formatDate $ addDays (-1) startDate,
			"--until", formatDate $ addDays 1 startDate,
	--		"--author=\"" ++ username ++ "\"",
			"--stat"])
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
			putStrLn $ "GIT: parse error: " ++ Util.displayErrors pe
			return []
		Right x -> do
			let myCommits = filter ((isInfixOf username) . commitAuthor) x
			let myCommitsInInterval = filter (inRange . localDay . commitDate) myCommits
			return $ map (toEvent project timezone) myCommitsInInterval
	where
		inRange date = (date >= startDate && date < (addDays 1 startDate))
	
toEvent :: T.Text -> TimeZone -> Commit -> Event.Event
toEvent project timezone commit =
	Event.Event
		{
			Event.eventDate = (localTimeToUTC timezone (commitDate commit)),
			Event.eventType = Event.Svn,
			Event.project = (Just $ T.unpack project),
			Event.desc = commitDesc commit,
			Event.extraInfo = (T.pack $ Util.getFilesRoot $ commitFiles commit),
			Event.fullContents = Just $ T.pack $ commitContents commit
		}

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
		commitFiles :: [String],
		commitAuthor :: String,
		commitContents :: String
	}
	deriving (Eq, Show)

parseCommits :: T.GenParser st [Commit]
parseCommits = many parseCommit

parseCommit :: T.GenParser st Commit
parseCommit = do
	string "commit "
	readLine
	string "Author: "
	author <- readLine
	date <- parseDateTime
	eol
	summary <- parseSummary
	count 2 eol
	cFiles <- parseFiles
	let cFileNames = fmap snd cFiles
	let cFilesDesc = fmap fst cFiles
	optional eol
	return $ Commit
		{
			commitDate = date,
			commitDesc = T.strip $ T.pack summary,
			commitFiles = cFileNames,
			commitAuthor = T.unpack $ T.strip $ T.pack author,
			commitContents = "<pre>" ++ intercalate "<br/>\n" cFilesDesc ++ "</pre>"
		}

readLine :: T.GenParser st String
readLine = do
	result <- T.many $ T.noneOf "\r\n"
	T.oneOf "\r\n"
	return result

parseFiles :: T.GenParser st [(String, String)]
parseFiles = manyTill parseFile (T.try parseFilesSummary)

parseFile :: T.GenParser st (String, String)
parseFile = do
	char ' '
	result <- T.many $ T.noneOf "|"
	rest <- T.many $ T.noneOf "\n"
	eol
	return (result ++ rest, T.unpack $ T.strip $ T.pack result)

parseFilesSummary :: T.GenParser st String
parseFilesSummary = do
	char ' '
	many1 digit
	string " file"
	T.many $ T.noneOf "\n"
	eol

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	string "Date:"
	many $ T.char ' '
	count 3 T.anyChar -- day
	T.char ' '
	month <- count 3 T.anyChar
	T.char ' '
	dayOfMonth <- T.many1 $ T.noneOf " "
	T.char ' '
	hour <- count 2 digit
	T.char ':'
	mins <- count 2 digit
	T.char ':'
	seconds <- count 2 digit
	T.char ' '
	year <- count 4 digit
	T.char ' '
	oneOf "-+"
	count 4 digit
	return $ LocalTime
		(fromGregorian (Util.parsedToInteger year) (strToMonth month) (Util.parsedToInt dayOfMonth))
		(TimeOfDay (Util.parsedToInt hour) (Util.parsedToInt mins) (fromIntegral $ Util.parsedToInt seconds))

strToMonth :: String -> Int
strToMonth month = case month of
	"Jan" -> 1
	"Feb" -> 2
	"Mar" -> 3
	"Apr" -> 4
	"May" -> 5
	"Jun" -> 6
	"Jul" -> 7
	"Aug" -> 8
	"Sep" -> 9
	"Oct" -> 10
	"Nov" -> 11
	"Dec" -> 12
	_ -> error $ "Unknown month " ++ month

parseSummary :: T.GenParser st String
parseSummary = manyTill anyChar (T.try $ string "\n\n" <|> string "\r\n\r\n")

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"
