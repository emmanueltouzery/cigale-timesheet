{-# LANGUAGE OverloadedStrings #-}

module Svn where

import qualified System.Process as Process
import qualified System.IO as IO
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import Text.Regex
import Data.Maybe
import Debug.Trace

getRepoCommits :: String -> Day -> Day -> IO [Commit]
getRepoCommits url startDate endDate = do
	let dateRange = formatDateRange startDate endDate
	(inh, Just outh, errh, pid) <- Process.createProcess (Process.proc "svn" ["log", url, "-r", dateRange]) {Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	print $ lines output
	return $ parseCommits $ lines output

data Commit = Commit
	{
		-- date :: UTCTime
		date :: String
	}
	deriving (Eq, Show)

parseCommits :: [String] -> [Commit]
parseCommits [] = []
parseCommits (a:[]) = [] -- in the end only the separator is left.
-- skip the first line which is "----.."
parseCommits (separator:x:xs) = trace x (Commit (headerInfo !! 2)) : (parseCommits $ drop (linesCount+1) xs)
	where
		linesCount = read (headerInfo !! 3) :: Int
		headerInfo = fromJust $ matchRegex headerRegex x
		headerRegex = mkRegex "r([0-9]+)[ \t]*\\|([^\\|]+)\\|([^\\|]+)\\|[ \t]*([0-9]+) line.*"

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day
