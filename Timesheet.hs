{-# LANGUAGE OverloadedStrings #-}

import qualified Svn
import qualified Email

import System.Environment
import System.Exit
import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO
import Data.Text.Read
import qualified Data.Map as Map
import Data.Maybe

import qualified Util
import qualified Event

svnUser = "emmanuelt"
svnByProjects = Map.fromList [ ("ADRIA", ["https://svn2.redgale.com/ak"]) ]

main = do
	args <- getArgs
	case length args of
		1 -> process $ T.pack $ head args
		otherwise -> do
			putStrLn "Parameters: <month to get the data - 2012-12 for instance>"
			exitFailure

getSvnEvents :: Day -> Day -> IO [Event.Event]
getSvnEvents firstDayOfMonth lastDayOfMonth = do
		let fetchCommits = Svn.getRepoCommits svnUser firstDayOfMonth lastDayOfMonth
		let projRepos = [(projectName, svnRepo)
				| projectName <- Map.keys svnByProjects,
				  svnRepo <- fromJust $ Map.lookup projectName svnByProjects ]
		commits <- sequence $ fmap (uncurry fetchCommits) projRepos
		return $ foldr (++) [] commits

process :: T.Text -> IO ()
process monthStr = do
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let firstDayOfMonth = fromGregorian (toInteger $ head ymd) (ymd !! 1) 1
	let firstDayNextMonth = addGregorianMonthsClip 1 firstDayOfMonth
	let lastDayOfMonth = addDays (-1) firstDayNextMonth
	svnEvents <- getSvnEvents firstDayNextMonth lastDayOfMonth
	--commits <- Svn.getRepoCommits repoUrl svnUser firstDayOfMonth lastDayOfMonth
	emails <- Email.getEmails firstDayOfMonth lastDayOfMonth
	--fileH <- openFile ((T.unpack monthStr) ++ ".json") WriteMode
	--BL.hPut fileH (JSON.encode commits)
	--hClose fileH
	putStrLn "done!"
