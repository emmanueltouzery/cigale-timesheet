{-# LANGUAGE OverloadedStrings #-}
module Timesheet where

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Read
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import GHC.Exts

import Control.Concurrent.Async

import qualified Util
import qualified Event
import qualified Ical
import qualified Svn
import qualified Email
import qualified Hg

svnUser :: T.Text
svnUser = "emmanuelt"

svnByProjects :: Map.Map String [String]
svnByProjects = Map.fromList [ ("ADRIA", ["https://svn2.redgale.com/ak"]),
				("METREL", ["https://svn2.redgale.com/met"]),
				("BUS", ["https://svn2.redgale.com/android"])]

hgUser = "Emmanuel Touzery"

hgFoldersByProjects = Map.fromList [ ("BUS", ["C:/projects/bus/smrt/src/ecodriving"]) ]

emailsByProjects :: [(Event.Project, [T.Text])]
emailsByProjects = [ ("ADRIA", ["@adriakombi.si"]), ("METREL", ["@metrel.si"])]

-- main :: IO ()
-- main = do
-- 	args <- getArgs
-- 	case length args of
-- 		1 -> process $ T.pack $ head args
-- 		_ -> do
-- 			putStrLn "Parameters: <month to get the data - 2012-12 for instance>"
-- 			exitFailure

getSvnEvents :: Day -> Day -> IO [Event.Event]
getSvnEvents firstDayOfMonth lastDayOfMonth = do
	let projRepos = [(projectName, svnRepo)
			| projectName <- Map.keys svnByProjects,
			  svnRepo <- fromJust $ Map.lookup projectName svnByProjects ]
	commits <- mapConcurrently (uncurry fetchCommits) projRepos
	return $ foldr (++) [] commits
	where fetchCommits = Svn.getRepoCommits svnUser firstDayOfMonth lastDayOfMonth

getHgEvents :: Day -> Day -> IO [Event.Event]
getHgEvents firstDayOfMonth lastDayOfMonth = do
	let projFolders = [(projectName, hgFolder)
			| projectName <- Map.keys hgFoldersByProjects,
			  hgFolder <- fromJust $ Map.lookup projectName hgFoldersByProjects ]
	commits <- mapConcurrently (uncurry fetchCommits) projFolders
	return $ foldr (++) [] commits
	where fetchCommits = Hg.getRepoCommits hgUser firstDayOfMonth lastDayOfMonth

getEmailEvents :: Day -> Day -> IO [Event.Event]
getEmailEvents firstDayOfMonth lastDayOfMonth = do
	emails <- Email.getEmails firstDayOfMonth lastDayOfMonth
	return $ map toEvent emails

toEvent :: Email.Email -> Event.Event
toEvent email = Event.Event
			{
				Event.eventDate = Email.date email,
				Event.eventType = Event.Email,
				Event.project  = getEmailProject email,
				Event.extraInfo = Email.subject email
			}

getEmailProject :: Email.Email -> Maybe Event.Project
getEmailProject email = fmap fst $ find ((isEmailInProject email) . snd) emailsByProjects

isEmailInProject :: Email.Email -> [T.Text] -> Bool
isEmailInProject email addressList = not . null $ filter emailMatches addressList
	where
		emailMatches address = address `T.isInfixOf` emailToCc
		emailToCc = T.concat [Email.to email, maybe "" id (Email.cc email)]


process :: T.Text -> IO BL.ByteString
process monthStr = do
	-- TODO this will fail with cryptic error messages if not given
	-- a string by the right format!
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let date = fromGregorian (toInteger $ head ymd) (ymd !! 1) (ymd !! 2)
	svnEvents <- getSvnEvents date date
	hgEvents <- getHgEvents date date
	emailEvents <- getEmailEvents date date
	icalEvents <- Ical.getCalendarEvents date date
	let allEvents = svnEvents ++ hgEvents ++ emailEvents ++ icalEvents
	let sortedEvents = sortWith Event.eventDate allEvents
	return $ JSON.encode sortedEvents 
