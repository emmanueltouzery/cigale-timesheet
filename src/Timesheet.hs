{-# LANGUAGE OverloadedStrings #-}
module Timesheet where

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Read
import Data.List
import Data.Time
import Data.Maybe

import GHC.Exts

import Control.Concurrent.Async

import qualified Util
import qualified Event
import qualified Ical
import qualified Svn
import qualified Email
import qualified Hg
import qualified Git
import Skype
import qualified Config

-- main :: IO ()
-- main = do
-- 	args <- getArgs
-- 	case length args of
-- 		1 -> process $ T.pack $ head args
-- 		_ -> do
-- 			putStrLn "Parameters: <month to get the data - 2012-12 for instance>"
-- 			exitFailure

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(a, b, c) -> f a b c

getSvnEvents :: [Config.SvnRecord] -> Day -> IO [Event.Event]
getSvnEvents svnRecords day = do
	commits <- mapConcurrently (uncurry3 fetchCommits) (fmap svnToTuple svnRecords)
	return $ concat commits
	where
		fetchCommits = Svn.getRepoCommits day day
		svnToTuple (Config.SvnRecord p u r) = (u, p, r)

getHgEvents :: [Config.HgRecord] -> Day -> IO [Event.Event]
getHgEvents hgRecords day = do
	commits <- mapConcurrently (uncurry3 fetchCommits) (fmap hgToTuple hgRecords)
	return $ concat commits
	where
		fetchCommits = Hg.getRepoCommits day
		hgToTuple (Config.HgRecord p u r) = (u, p, r)

getGitEvents :: [Config.GitRecord] -> Day -> IO [Event.Event]
getGitEvents gitRecords day = do
	commits <- mapConcurrently (uncurry3 fetchCommits) (fmap gitToTuple gitRecords)
	return $ concat commits
	where
		fetchCommits = Git.getRepoCommits day
		gitToTuple (Config.GitRecord p u r) = (u, p, r)

getEmailEvents :: Config.EmailConfig -> Day -> IO [Event.Event]
getEmailEvents emailConfig day = do
	let mboxLocations = fmap T.unpack (Config.emailPaths emailConfig)
	emailsAr <- mapM (\mbox -> Email.getEmails mbox day day) mboxLocations
	let emails = concat emailsAr
	let emailRecords = Config.emailRecords emailConfig
	timezone <- getCurrentTimeZone
	return $ map (toEvent emailRecords timezone) emails

toEvent :: [Config.EmailRecord] -> TimeZone -> Email.Email -> Event.Event
toEvent emailRecords timezone email = Event.Event
			{
				Event.eventDate = localTimeToUTC timezone (Email.date email),
				Event.eventType = Event.Email,
				Event.project  = getEmailProject email emailRecords,
				Event.desc = Email.subject email,
				Event.extraInfo = T.concat["to: ", Email.to email],
				Event.fullContents = Just $ Email.contents email
			}

getEmailProject :: Email.Email -> [Config.EmailRecord] -> Maybe Event.Project
getEmailProject email emailRecords = fmap (T.unpack . Config.emailProj) maybeRecordForEmail
	where
		maybeRecordForEmail = find isEmailInRecord emailRecords
		isEmailInRecord = (emailMatchesPatterns email) . Config.emailPatterns

emailMatchesPatterns :: Email.Email -> [T.Text] -> Bool
emailMatchesPatterns email addressList = not . null $ filter emailMatches addressList
	where
		emailMatches address = address `T.isInfixOf` emailToCc
		emailToCc = T.concat [Email.to email, fromMaybe "" (Email.cc email)]


process :: T.Text -> IO BL.ByteString
process monthStr = do
	mayConfig <- Config.readConfig
	case mayConfig of
		Nothing -> do putStrLn "Configuration problem, check config.json"; return "Configuration problem, check config.json"
		Just config -> processConfig monthStr config

processConfig :: T.Text -> Config.ActivityConfig -> IO BL.ByteString
processConfig monthStr config = do
	-- TODO this will fail with cryptic error messages if not given
	-- a string by the right format!
	myTz <- getCurrentTimeZone
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let date = fromGregorian (toInteger $ head ymd) (ymd !! 1) (ymd !! 2)
	putStrLn "fetching from SVN"
	svnEvents <- getSvnEvents (Config.svn config) date
	putStrLn $ "found " ++ (show $ length svnEvents) ++ " SVN events."
	putStrLn "fetching from HG"
	hgEvents <- getHgEvents (Config.hg config) date
	putStrLn $ "found " ++ (show $ length hgEvents) ++ " HG events."
	putStrLn "fetching from GIT"
	gitEvents <- getGitEvents (Config.git config) date
	putStrLn $ "found " ++ (show $ length gitEvents) ++ " GIT events."
	putStrLn "fetching from email"
	emailEvents <- getEmailEvents (Config.email config) date
	putStrLn $ "found " ++ (show $ length emailEvents) ++ " email events."
	putStrLn "fetching from ical"
	icalEvents <- Ical.getCalendarEvents (T.unpack $ Config.icalUrl $ head $ Config.ical config) date date
	putStrLn $ "found " ++ (show $ length icalEvents) ++ " calendar events."
	putStrLn "fetching from Skype"
	skypeEvents <- getSkypeEvents date (Config.skypeUsername $ Config.skype config)
	putStrLn $ "found " ++ (show $ length skypeEvents) ++ " skype events."
	putStrLn "done!"
	let allEvents = svnEvents ++ hgEvents ++ gitEvents ++ emailEvents ++ icalEvents ++ skypeEvents
	let sortedEvents = sortWith Event.eventDate allEvents
	let eventDates = fmap Event.eventDate sortedEvents
	let eventDatesLocal = fmap (utcToLocalTime myTz) eventDates
	-- well would be faster to just check the first and last
	-- element... but it's actually shorter to code like this..
	let outOfRangeData = filter (outOfRange date (addDays 1 date)) eventDatesLocal
	let ok = null outOfRangeData
	if ok
		then return $ JSON.encode sortedEvents 
		else do
			putStrLn "*** SOME EVENTS ARE NOT IN TIME RANGE"
			print outOfRangeData
			print $ head sortedEvents
			print $ last sortedEvents
			return BL.empty
	where
		outOfRange start end time = time < (LocalTime start midnight) || time > (LocalTime end midnight)
