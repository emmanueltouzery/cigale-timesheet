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

import qualified Timesheet
import qualified Settings
import Config (getConfigFileName, addPluginInConfig, updatePluginInConfig, deletePluginFromConfig)
import Util (toStrict1)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "index.html") <|>
    route [ ("timesheet/:tsparam", timesheet),
            ("configdesc", configdesc),
            ("config", method GET (serveFile "config.html")),
            ("configVal", configVal),
            ("configUpdate", configUpdate), -- TODO remove this
            ("config", method POST addConfigEntry),
            ("config", method PUT updateConfigEntry),
            ("config", method DELETE deleteConfigEntry)
          ] <|>
    dir "static" (serveDirectory ".")

timesheet :: Snap ()
timesheet = do
    modifyResponse $ setContentType "application/json"
    setTimeout 3600
    tsparam <- getParam "tsparam"
    maybe (writeBS "must specify the month and year in URL, like so: /timesheet/2012-11")
          handleTimesheet tsparam

handleTimesheet tsparam = do
	jsonData <- liftIO $ Timesheet.process $ TE.decodeUtf8 tsparam
	liftIO $ putStrLn $ "OK i have all the json" ++ (show $ DBLC.length jsonData)
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
		then serveFile $ settingsFile
		else writeLBS "{}"

configUpdate :: Snap ()
configUpdate = do
	configFileName <- liftIO Config.getConfigFileName
	liftIO $ renameFile configFileName (configFileName ++ ".bak")
	outH <- liftIO $ openFile configFileName WriteMode
	requestBody <- readRequestBody 64000
	liftIO $ hPut outH requestBody
	liftIO $ hClose outH

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
			configJson <- getRequestBody >>= return . toStrict1
			(liftIO $ handler _pName configJson) >>= setResponse
		_ -> setResponse $ Left "add config: pluginName not specified"

setResponse :: Either BS.ByteString BS.ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val
