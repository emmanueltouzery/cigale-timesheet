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
import Data.List

import qualified Util
import qualified Event

svnUser :: T.Text
svnUser = "emmanuelt"

svnByProjects :: Map.Map String [String]
svnByProjects = Map.fromList [ ("ADRIA", ["https://svn2.redgale.com/ak"]) ]

emailsByProjects :: [(Event.Project, [T.Text])]
emailsByProjects = [ ("ADRIA", ["@adriakombi.si"]), ("METREL", ["@metrel.si"])]

main :: IO ()
main = do
	args <- getArgs
	case length args of
		1 -> process $ T.pack $ head args
		_ -> do
			putStrLn "Parameters: <month to get the data - 2012-12 for instance>"
			exitFailure

getSvnEvents :: Day -> Day -> IO [Event.Event]
getSvnEvents firstDayOfMonth lastDayOfMonth = do
	let projRepos = [(projectName, svnRepo)
			| projectName <- Map.keys svnByProjects,
			  svnRepo <- fromJust $ Map.lookup projectName svnByProjects ]
	commits <- sequence $ fmap (uncurry fetchCommits) projRepos
	return $ foldr (++) [] commits
	where fetchCommits = Svn.getRepoCommits svnUser firstDayOfMonth lastDayOfMonth

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


process :: T.Text -> IO ()
process monthStr = do
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let firstDayOfMonth = fromGregorian (toInteger $ head ymd) (ymd !! 1) 1
	let firstDayNextMonth = addGregorianMonthsClip 1 firstDayOfMonth
	let lastDayOfMonth = addDays (-1) firstDayNextMonth
	svnEvents <- getSvnEvents firstDayNextMonth lastDayOfMonth
	--commits <- Svn.getRepoCommits repoUrl svnUser firstDayOfMonth lastDayOfMonth
	--emails <- Email.getEmails firstDayOfMonth lastDayOfMonth
	emailEvents <- getEmailEvents firstDayOfMonth lastDayOfMonth
	putStrLn $ "email events " ++ (show $ length emailEvents)
	fileH <- openFile ((T.unpack monthStr) ++ ".json") WriteMode
	BL.hPut fileH (JSON.encode $ svnEvents ++ emailEvents)
	hClose fileH
	putStrLn "done!"
