{-# LANGUAGE OverloadedStrings #-}

module Hg where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import qualified Event
import qualified Util

getRepoCommits :: Day -> T.Text -> T.Text -> T.Text -> IO [Event.Event]
getRepoCommits startDate _username project _projectPath = do
	let username = T.unpack _username
	let projectPath = T.unpack _projectPath
	let dateRange = formatDate startDate
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "hg" [
			"log", "-k", username, "-d", dateRange,
			"--template", "{date|isodate}\n{desc}\n--->>>\n"])
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
			putStrLn $ "HG: parse error: " ++ displayErrors pe
			return []
		Right x -> return $ map (uncurry $ toEvent project timezone) x
	where
		displayErrors pe = concat $ fmap messageString (errorMessages pe)
	
toEvent :: T.Text -> TimeZone -> LocalTime -> T.Text -> Event.Event
toEvent project timezone time summary = Event.Event (localTimeToUTC timezone time) Event.Svn (Just $ T.unpack project) summary

formatDate :: Day -> String
formatDate day =
	(show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth)
	where
		(year, month, dayOfMonth) = toGregorian day

-- i am lame here.. i don't know how to make a
-- parsec function taking parameters.
-- so instead of taking a parameter "project",
-- i return an array of functions taking that
-- parameter project.
-- TODO one day ask in haskell-beginners about this one.
parseCommitsParsec :: T.Text -> Either ParseError [(LocalTime, T.Text)]
parseCommitsParsec commits = parse parseCommits "" commits

parseCommits :: T.GenParser st [(LocalTime, T.Text)]
parseCommits = do
	many $ parseCommit

parseCommit :: T.GenParser st (LocalTime, T.Text)
parseCommit = do
	date <- parseDateTime
	summary <- parseSummary
	eol
	return (date, T.pack summary)

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
