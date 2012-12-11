module Svn where

import qualified System.Process as Process
import qualified System.IO as IO
import Data.Time.Clock
import Data.Time.Calendar

-- at first I started using VCSWrapper
-- but it didn't compile, neither for GHC 7.0
-- on fedora nor 7.4 on windows. I found a fixed
-- version but the patch wasn't accepted by the
-- maintainers yet (https://github.com/leksah/haskellVCSWrapper/tree/21fb59ff0b994193c98021fe8bcd628676e783ec)
-- but even that wasn't working for me, and
-- on top of that it works on a checkout while
-- i want to work on a whole repository...
-- so in the end i rolled my own, but a bit
-- inspired by VCSWrapper, taking some of their code.
-- VCSWrapper is GPLv2...

getRepoCommits :: String -> Day -> Day -> IO ()
getRepoCommits url startDate endDate = do
	let dateRange = formatDateRange startDate endDate
	(inh, Just outh, errh, pid) <- Process.createProcess (Process.proc "svn" ["log", url, "-r", dateRange]) {Process.std_out = Process.CreatePipe}
	ex <- Process.waitForProcess pid
	output <- IO.hGetContents outh
	putStrLn $ show $ length output
	return ()

formatDateRange :: Day -> Day -> String
formatDateRange startDate endDate =
	formatDate startDate ++ ":" ++ formatDate endDate

formatDate :: Day -> String
formatDate day =
	"{" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth) ++ "}"
	where
		(year, month, dayOfMonth) = toGregorian day
