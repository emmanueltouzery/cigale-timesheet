{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns #-}

module Svn where

import qualified System.Process as Process
import qualified System.IO as IO
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import Text.Regex.PCRE.Rex
import Data.Maybe
import Data.String.Utils

getRepoCommits :: String -> Day -> Day -> IO [Commit]
getRepoCommits url startDate endDate = do
	let dateRange = formatDateRange startDate endDate
	(inh, Just outh, errh, pid) <- Process.createProcess (Process.proc "svn" ["log", url, "-r", dateRange]) {Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
--	print $ lines output
	return $ parseCommits $ lines output

data Commit = Commit
	{
		-- date :: UTCTime
		revision :: Int,
		date :: String,
		user :: String,
		linesCount :: Int
	}
	deriving (Eq, Show)

parseCommits :: [String] -> [Commit]
parseCommits [] = []
parseCommits (a:[]) = [] -- in the end only the separator is left.
-- skip the first line which is "----.."
parseCommits (separator:x:xs) = commit : (parseCommits $ drop ((linesCount commit)+1) xs)
	where
		commit = parseSingleCommit x

parseSingleCommit :: String -> Commit
-- i have to use \x7C instead of | otherwise I get the pattern [^|] and |] is
-- exactly the terminator for quasiquoting, causing a mess.
parseSingleCommit [rex|r(?{read -> revision}\d+)\s*\|\s*(?{strip -> user}[^\x7C]+)\s*\|\s*(?{date}[^\x7C]+)\s*\|\s*(?{read->linesCount}\d+)\s+line|] = Commit revision date user linesCount

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day
