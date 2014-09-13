{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DoAndIfThenElse, LambdaCase, ViewPatterns #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Process (rawSystem)
import Control.Applicative
import Network.TCP (openTCPPort)
import Network.Stream (close)
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Error
import System.IO.Error
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Data.Time
import Control.Monad.Trans
import Control.Monad
import Data.List
import Data.Aeson
import Data.Char (ord)
import System.Environment (getArgs)
import Text.Printf

import qualified Timesheet
import Config
import Util (parse2, parseNum, parseMaybe)
import Paths_cigale_timesheet
import FilePickerServer (browseFolder)
import SnapUtil (setActionResponse, hParam, noteET)
import qualified EventProviders (plugins)
import EventProvider

appPort :: Int
appPort = 8000

main :: IO ()
main = do
	args <- getArgs
	getPrefetchFolder >>= createDirectoryIfMissing True
	removeObsoletePrefetch
	if args == ["--prefetch"]
		then doPrefetch
		else startWebApp

doPrefetch :: IO ()
doPrefetch = do
	prefetchFolder <- getPrefetchFolder
	today <- getToday
	-- fetch from the first day of the previous month
	let prefetchStart = addGregorianMonthsClip (-1)
		$ (\(y,m,_) -> fromGregorian y m 1) $ toGregorian today
	-- now fetch from oldestAccepted to yesterday, if needed.
	prefetch prefetchFolder prefetchStart (addDays (-1) today)

getToday :: IO Day
getToday = (localDay . zonedTimeToLocalTime) <$> getZonedTime

removeObsoletePrefetch :: IO ()
removeObsoletePrefetch = do
	-- remove obsolete prefetch files, that means from
	-- let's say 3 months ago.
	oldestAccepted <- addGregorianMonthsClip (-3) <$> getToday
	prefetchFolder <- getPrefetchFolder
	getDirectoryContents prefetchFolder >>= mapM_ (removeIfOlderThan oldestAccepted prefetchFolder)
	
prefetch :: FilePath -> Day -> Day -> IO ()
prefetch folder curDay maxDay
	| curDay > maxDay = return ()
	| otherwise = unlessM (doesFileExist fname) $ do
		fetchTimesheetAndStore curDay fname
		prefetch folder (addDays 1 curDay) maxDay
	where
		fname = folder </> getPrefetchFilename curDay
		unlessM s r = not <$> s >>= flip when r

getPrefetchFolder :: IO FilePath
getPrefetchFolder = (++ "/prefetch") <$> Config.getSettingsFolder

getPrefetchFilename :: Day -> FilePath
getPrefetchFilename (toGregorian -> (y,m,d)) = printf "%d-%02d-%02d.json" y m d

removeIfOlderThan :: Day -> FilePath -> FilePath -> IO ()
removeIfOlderThan date folder filename =
	case parseMaybe parsePrefetchFilename (T.pack filename) of
		Nothing -> return () -- not a prefetch file.
		Just fileDate -> when (fileDate < date) $ removeFile $ folder </> filename

parsePrefetchFilename :: T.GenParser st Day
parsePrefetchFilename = parseDate <* T.string ".json"

startWebApp :: IO ()
startWebApp = do
	installPath <- Paths_cigale_timesheet.getDataFileName ""
	let snapConfig = setPort appPort .
		setAccessLog ConfigNoLog .
		setErrorLog ConfigNoLog $
		defaultConfig
	forkIO openInBrowser
	httpServe snapConfig (site installPath)

-- wait for the port to be opened then
-- start the browser on the URL of the app.
-- If we start right away the server may
-- not be ready.
openInBrowser :: IO ()
openInBrowser = do
	portOpen <- try (openTCPPort "127.0.0.1" appPort)
	case portOpen of
		Left (_ :: SomeException) -> openInBrowser
		Right conn -> close conn >> openApp

openApp :: IO ()
openApp = do
	epiphany <- hasProgram "epiphany"
	void $ if epiphany
	then do
		settingsFolder <- Config.getSettingsFolder
		let profileDir = settingsFolder ++ "/epiphany-profile-app-cigale-timesheet"
		createDirectoryIfMissing True profileDir
		rawSystem "epiphany" ["--application-mode", "--profile=" ++ profileDir, url]
	else rawSystem "xdg-open" [url]
	where
		url = "http://localhost:" ++ show appPort

hasProgram :: String -> IO Bool
hasProgram prog = isJust <$> findExecutable prog

site :: FilePath -> Snap ()
site installPath =
    ifTop (serveFile $ installPath ++ "/FayApp.html") <|>
    route [ ("timesheet/:tsparam", timesheet),
            ("config", method GET (serveFile $ installPath ++ "/FayConfig.html")),
            ("configdesc", configdesc),
            ("configVal", configVal),
            ("config", method POST addConfigEntry),
            ("config", method PUT updateConfigEntry),
            ("config", method DELETE deleteConfigEntry),
            ("browseFolder", browseFolder),
	    ("getExtraData", httpGetExtraData)
          ] <|>
    dir "static" (serveDirectory installPath)

timesheet :: Snap ()
timesheet = setActionResponse $ do
	lift $ do
		modifyResponse $ setContentType "application/json"
		setTimeout 3600
	dateParamText <- TE.decodeUtf8 <$> hParam "tsparam"
	date <- hoistEither $ fmapL BS8.pack $ parse2 parseDate
		"Invalid date format, expected yyyy-mm-dd" dateParamText
	pFname <- (</> getPrefetchFilename date) <$> liftIO getPrefetchFolder
	-- first try to read from prefetch, if it fails, exception, calculate.
	liftIO $ BS.readFile pFname `catch` handleError date pFname
	where handleError date pFname e
		| isDoesNotExistError e = fetchTimesheetAndStore date pFname
		| otherwise = print e >> throwIO e

fetchTimesheetAndStore :: Day -> FilePath -> IO BS.ByteString
fetchTimesheetAndStore date fname = do
	contents <- Timesheet.process date
	today <- getToday
	-- only cache past dates
	when (date < today)
		$ withFile fname WriteMode $ \h -> BSL.hPut h contents
	return $ BSL.toStrict contents

parseDate :: T.GenParser st Day
parseDate = fromGregorian <$> parseNum 4
	<*> (T.char '-' >> parseNum 2) <*> (T.char '-' >> parseNum 2)

configdesc :: Snap ()
configdesc = do
	modifyResponse $ setContentType "application/json"
	writeLBS Timesheet.getEventProvidersConfig

configVal :: Snap ()
configVal = do
	modifyResponse $ setContentType "application/json"
	settingsFile <- liftIO Config.getConfigFileName
	isSettings <- liftIO $ doesFileExist settingsFile
	if isSettings
		then serveFile settingsFile
		else writeLBS "[]"

addConfigEntry :: Snap ()
addConfigEntry = setActionResponse $ do
	configItemJson <- lift (BSL.toStrict <$> readRequestBody 65536)
	liftIO $ addPluginInConfig configItemJson
	return ""

deleteConfigEntry :: Snap ()
deleteConfigEntry = setActionResponse $ do
	pluginName <- hParam "pluginName"
	liftIO (deletePluginFromConfig pluginName) >>= hoistEither

updateConfigEntry :: Snap ()
updateConfigEntry = setActionResponse $ processConfigFromBody updatePluginInConfig

processConfigFromBody :: (BS.ByteString -> IO (Either BS.ByteString BS.ByteString)) ->
		 EitherT BS.ByteString Snap BS.ByteString
processConfigFromBody handler = do
	pluginName <- hParam "pluginName"
	liftIO (handler pluginName) >>= hoistEither

httpGetExtraData :: Snap ()
httpGetExtraData = setActionResponse $ do
	cfgItemName <- TE.decodeUtf8 <$> hParam "configItemName"
	queryParams <- hParam "queryParams"
	config <- liftIO $ readConfig EventProviders.plugins
	eventSource <- noteET "no such config item" $ find ((==cfgItemName) . srcName) config
	extraData <- noteET "No extra data" $ getExtraData $ srcProvider eventSource
	decodedParam <- noteET "Error decoding queryParams" $ decodeStrict' queryParams
	settings <- liftIO Timesheet.getGlobalSettings
	extraDataResult <- liftIO $ extraData (srcConfig eventSource) settings decodedParam
	(contentType, contents) <- noteET "No extra data retrieved" extraDataResult
	lift $ modifyResponse $ setContentType $ BS.pack $ (fromIntegral . ord) <$> contentType
	return contents
