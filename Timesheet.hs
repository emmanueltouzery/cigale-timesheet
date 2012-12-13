{-# LANGUAGE OverloadedStrings #-}

import qualified Svn

import System.Environment
import System.Exit
import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL

-- TODO must filter by user too...
repoUrl = "https://svn2.redgale.com/ak"

main = do
	args <- getArgs
	case length args of
		1 -> process $ T.pack $ head args
		otherwise -> do
			putStrLn "Parameters: <month to get the data - 2012-12 for instance>"
			exitFailure


process :: T.Text -> IO ()
process monthStr = do
	let ymd = map toInt (T.splitOn "-" monthStr)
	let firstDayOfMonth = fromGregorian (toInteger $ head ymd) (ymd !! 1) 1
	let firstDayNextMonth = addGregorianMonthsClip 1 firstDayOfMonth
	let lastDayOfMonth = addDays (-1) firstDayNextMonth
	commits <- Svn.getRepoCommits repoUrl firstDayOfMonth lastDayOfMonth
	BL.putStrLn $ JSON.encode commits
	where
		toInt s = read (T.unpack s)
