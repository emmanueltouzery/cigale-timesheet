{-# LANGUAGE OverloadedStrings #-}
module Timesheet where

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Read
import Data.List

import GHC.Exts

import Control.Concurrent.Async

import qualified Util
import qualified Event
import qualified Ical
import qualified Svn
import qualified Email
import qualified Hg
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

getSvnEvents :: [Config.SvnRecord] -> Day -> Day -> IO [Event.Event]
getSvnEvents svnRecords firstDayOfMonth lastDayOfMonth = do
	commits <- mapConcurrently (uncurry3 fetchCommits) (fmap svnToTuple svnRecords)
	return $ foldr (++) [] commits
	where
		fetchCommits = Svn.getRepoCommits firstDayOfMonth lastDayOfMonth
		svnToTuple (Config.SvnRecord p u r) = (u, p, r)

getHgEvents :: [Config.HgRecord] -> Day -> Day -> IO [Event.Event]
getHgEvents hgRecords firstDayOfMonth lastDayOfMonth = do
	commits <- mapConcurrently (uncurry3 fetchCommits) (fmap hgToTuple hgRecords)
	return $ foldr (++) [] commits
	where
		fetchCommits = Hg.getRepoCommits firstDayOfMonth lastDayOfMonth
		hgToTuple (Config.HgRecord p u r) = (u, p, r)

getEmailEvents :: Config.EmailConfig -> Day -> Day -> IO [Event.Event]
getEmailEvents emailConfig firstDayOfMonth lastDayOfMonth = do
	let mboxLocations = fmap T.unpack (Config.emailPaths emailConfig)
	emailsAr <- sequence $ map (\mbox -> Email.getEmails mbox firstDayOfMonth lastDayOfMonth) mboxLocations
	let emails = foldr (++) [] emailsAr
	let emailRecords = Config.emailRecords emailConfig
	return $ map (toEvent emailRecords) emails

toEvent :: [Config.EmailRecord] -> Email.Email -> Event.Event
toEvent emailRecords email = Event.Event
			{
				Event.eventDate = Email.date email,
				Event.eventType = Event.Email,
				Event.project  = getEmailProject email emailRecords,
				Event.extraInfo = Email.subject email
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
		emailToCc = T.concat [Email.to email, maybe "" id (Email.cc email)]


process :: T.Text -> IO BL.ByteString
process monthStr = do
	mayConfig <- Config.readConfig
	case mayConfig of
		Nothing -> return "Configuration problem, check config.json"
		Just config -> processConfig monthStr config

processConfig :: T.Text -> Config.ActivityConfig -> IO BL.ByteString
processConfig monthStr config = do
	print config
	-- TODO this will fail with cryptic error messages if not given
	-- a string by the right format!
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let date = fromGregorian (toInteger $ head ymd) (ymd !! 1) (ymd !! 2)
	putStrLn "fetching from SVN"
	svnEvents <- getSvnEvents (Config.svn config) date date
	putStrLn $ "found " ++ (show $ length svnEvents) ++ " SVN events."
	putStrLn "fetching from HG"
	hgEvents <- getHgEvents (Config.hg config) date date
	putStrLn $ "found " ++ (show $ length hgEvents) ++ " HG events."
	putStrLn "fetching from email"
	emailEvents <- getEmailEvents (Config.email config) date date
	putStrLn $ "found " ++ (show $ length emailEvents) ++ " email events."
	putStrLn "fetching from ical"
	icalEvents <- Ical.getCalendarEvents date date
	putStrLn $ "found " ++ (show $ length icalEvents) ++ " calendar events."
	putStrLn "done!"
	let allEvents = svnEvents ++ hgEvents ++ emailEvents ++ icalEvents
	let sortedEvents = sortWith Event.eventDate allEvents
	return $ JSON.encode sortedEvents 
