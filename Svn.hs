{-# LANGUAGE QuasiQuotes, OverloadedStrings, DeriveGeneric #-}

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

getRepoCommits :: String -> T.Text -> Day -> Day -> IO [Commit]
getRepoCommits url username startDate endDate = do
	let dateRange = formatDateRange startDate endDate
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "svn" ["log", url, "-r", dateRange])
		{Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	let commits = parseCommits $ T.lines output
	return $ filter ((==username) . user) commits

data Commit = Commit
	{
		revision :: T.Text,
		date :: T.Text,
		user :: T.Text,
		linesCount :: Int,
		comment :: T.Text
	}
	deriving (Eq, Show, Generic)

instance JSON.ToJSON Commit

parseCommits :: [T.Text] -> [Commit]
parseCommits [] = []
parseCommits (a:[]) = [] -- in the end only the separator is left.
-- skip the first line which is "----.."
parseCommits (separator:commit_header:blank:xs) = commit : (parseCommits $ drop linesCount xs)
	where
		commit = Commit revision date user linesCount (T.unlines $ take linesCount xs)
		(revision:user:date:lines:[]) = map T.strip (T.splitOn "|" commit_header)
		linesCount = safePromise $ decimal (T.strip lines)
		safePromise (Right (v,_)) = v

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day
