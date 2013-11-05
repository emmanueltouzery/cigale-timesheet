{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as TE
import System.Directory
import System.IO
import Data.ByteString.Lazy as LBS (hPut, fromChunks, concat)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.String
import System.Process (rawSystem)
import Control.Monad (liftM)

import qualified Timesheet
import Config (getConfigFileName, addPluginInConfig, updatePluginInConfig, deletePluginFromConfig)
import Util (toStrict1)
import Paths_cigale_timesheet

main :: IO ()
main = do
	installPath <- Paths_cigale_timesheet.getDataFileName ""
	rawSystem "xdg-open" ["http://localhost:8000"]
	quickHttpServe (site installPath)

site :: FilePath -> Snap ()
site installPath =
    ifTop (serveFile $ installPath ++ "/FayApp.html") <|>
    route [ ("timesheet/:tsparam", timesheet),
            ("config", method GET (serveFile $ installPath ++ "/FayConfig.html")),
            ("configdesc", configdesc),
            ("configVal", configVal),
            ("config", method POST addConfigEntry),
            ("config", method PUT updateConfigEntry),
            ("config", method DELETE deleteConfigEntry)
          ] <|>
    dir "static" (serveDirectory installPath)

timesheet :: Snap ()
timesheet = do
    modifyResponse $ setContentType "application/json"
    setTimeout 3600
    tsparam <- getParam "tsparam"
    maybe (writeBS "must specify the month and year in URL, like so: /timesheet/2012-11")
          handleTimesheet tsparam

handleTimesheet tsparam = do
	jsonData <- liftIO $ Timesheet.process $ TE.decodeUtf8 tsparam
	liftIO $ putStrLn $ "OK i have all the json -- bytes: " ++ (show $ DBLC.length jsonData)
	writeLBS jsonData

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
addConfigEntry = processConfigFromBody addPluginInConfig 

deleteConfigEntry :: Snap ()
deleteConfigEntry = do
	mOldCfg <- getSingleParam "oldVal"
	mPluginName <- getSingleParam "pluginName"
	let mOldCfgPluginName = sequence [mOldCfg, mPluginName]
	case mOldCfgPluginName of
		Just (oldCfg:pName:[]) ->
			(liftIO $ deletePluginFromConfig oldCfg pName) >>= setResponse
		_ -> setResponse $ Left "delete: parameters missing"

updateConfigEntry :: Snap ()
updateConfigEntry = do
	oldCfg <- getSingleParam "oldVal"
	case oldCfg of
		Just _oldCfg -> processConfigFromBody (updatePluginInConfig _oldCfg)
		_ -> setResponse $ Left "update config: pluginName not specified"

getSingleParam :: BS.ByteString -> Snap (Maybe BS.ByteString)
getSingleParam pName = do
	rq <- getRequest 
	case rqParam pName rq of
		(Just (pVal:[])) -> return $ Just pVal
		_ -> return Nothing

processConfigFromBody :: (BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)) ->
		 Snap ()
processConfigFromBody handler = do
	pName <- getSingleParam "pluginName"
	case pName of
		Just _pName -> do
			configJson <- liftM toStrict1 getRequestBody
			(liftIO $ handler _pName configJson) >>= setResponse
		_ -> setResponse $ Left "add config: pluginName not specified"

setResponse :: Either BS.ByteString BS.ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val
