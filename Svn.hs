{-# LANGUAGE QuasiQuotes, OverloadedStrings, DeriveGeneric, ViewPatterns #-}

module Svn where

import qualified System.Process as Process
import qualified Data.Text.IO as IO
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import Data.Maybe
import Data.String.Utils
import Data.Char
import Debug.Trace
import qualified Data.Aeson as JSON
import GHC.Generics
import Data.Text.Read

import qualified Util
import qualified Event

import Text.Regex.PCRE.Rex

getRepoCommits :: T.Text -> Day -> Day -> String -> String -> IO [Event.Event]
getRepoCommits username startDate endDate projectName url = do
	let dateRange = formatDateRange startDate endDate
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "svn" ["log", url, "-r", dateRange])
		{Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	let commits = parseCommits $ T.lines output
	let myCommits = filter ((==username) . user) commits
	return $ map (toEvent projectName) myCommits

data Commit = Commit
	{
		revision :: T.Text,
		date :: UTCTime,
		user :: T.Text,
		linesCount :: Int,
		comment :: T.Text
	}
	deriving (Eq, Show, Generic)

instance JSON.ToJSON Commit

toEvent :: String -> Commit -> Event.Event
toEvent projectName (Commit _ dateVal _ _ commentVal) = Event.Event dateVal Event.Svn (Just projectName) commentVal

parseCommits :: [T.Text] -> [Commit]
parseCommits [] = []
parseCommits (_:[]) = [] -- in the end only the separator is left.
-- skip the first line which is "----.."
parseCommits (_:commit_header:_:xs) = commit : (parseCommits $ drop linesCnt xs)
	where
		commit = Commit rev dateVal usr linesCnt (T.unlines $ take linesCnt xs)
		dateVal = parseSvnDate $ T.unpack dateStr
		(rev:usr:dateStr:linesVal:[]) = map T.strip (T.splitOn "|" commit_header)
		linesCnt = Util.safePromise $ decimal (T.strip linesVal)
parseCommits _ = error "Should not happen"

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day


parseSvnDate :: String -> UTCTime
parseSvnDate [rex|(?{read -> year}\d+)-(?{read -> month}\d+)-
		(?{read -> day}\d+)\s(?{read -> hour}\d+):(?{read -> mins}\d+):
		(?{read -> sec}\d+)|] =
	UTCTime (fromGregorian year month day) (secondsToDiffTime (hour*3600+mins*60+sec))
