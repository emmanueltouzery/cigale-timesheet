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
toEvent projectName (Commit _ date _ _ comment) = Event.Event date Event.Svn (Just projectName) comment

parseCommits :: [T.Text] -> [Commit]
parseCommits [] = []
parseCommits (a:[]) = [] -- in the end only the separator is left.
-- skip the first line which is "----.."
parseCommits (separator:commit_header:blank:xs) = commit : (parseCommits $ drop linesCount xs)
	where
		commit = Commit revision date user linesCount (T.unlines $ take linesCount xs)
		date = parseSvnDate $ T.unpack dateStr
		(revision:user:dateStr:lines:[]) = map T.strip (T.splitOn "|" commit_header)
		linesCount = Util.safePromise $ decimal (T.strip lines)

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
		(?{read -> day}\d+)\s(?{read -> hour}\d+):(?{read -> min}\d+):
		(?{read -> sec}\d+)|] =
	UTCTime (fromGregorian year month day) (secondsToDiffTime (hour*3600+min*60+sec))
