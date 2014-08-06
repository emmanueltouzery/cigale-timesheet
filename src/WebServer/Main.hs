{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DoAndIfThenElse, LambdaCase #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Text.Encoding as TE
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Process (rawSystem)
import Control.Applicative
import Network.TCP (openTCPPort)
import Network.Stream (close)
import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Error
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Data.Time
import Control.Monad.Trans

import qualified Timesheet
import Config
import Util (toStrict1, parse2, parseNum)
import Paths_cigale_timesheet
import FilePickerServer (browseFolder)
import SnapUtil (setActionResponse, hParam)

appPort :: Int
appPort = 8000

main :: IO ()
main = do
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
	if epiphany
	then do
		settingsFolder <- Config.getSettingsFolder
		let profileDir = settingsFolder ++ "/epiphany-profile-app-cigale-timesheet"
		createDirectoryIfMissing True profileDir
		rawSystem "epiphany" ["--application-mode", "--profile=" ++ profileDir, url]
	else rawSystem "xdg-open" [url]
	return ()
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
            ("browseFolder", browseFolder)
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
	liftIO (toStrict1 <$> Timesheet.process date)

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
		else writeLBS "{}"

addConfigEntry :: Snap ()
addConfigEntry = setActionResponse $ processConfigFromBody addPluginInConfig

deleteConfigEntry :: Snap ()
deleteConfigEntry = setActionResponse $ do
	oldCfg <- hParam "oldVal"
	pluginName <- hParam "pluginName"
	liftIO (deletePluginFromConfig oldCfg pluginName) >>= hoistEither

updateConfigEntry :: Snap ()
updateConfigEntry = setActionResponse $ do
	oldCfg <- hParam "oldVal"
	processConfigFromBody $ updatePluginInConfig oldCfg

processConfigFromBody :: (BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)) ->
		 EitherT BS.ByteString Snap BS.ByteString
processConfigFromBody handler = do
	configJson <- lift (toStrict1 <$> readRequestBody 65536)
	pluginName <- hParam "pluginName"
	liftIO (handler pluginName configJson) >>= hoistEither
