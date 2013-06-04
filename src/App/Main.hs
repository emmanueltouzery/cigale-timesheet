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
import Data.ByteString.Lazy (hPut)

import qualified Timesheet
import qualified Settings
import qualified Config (getConfigFileName)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "index.html") <|>
    route [ ("timesheet/:tsparam", timesheet),
            ("configdesc", configdesc),
            ("config", serveFile "config.html"),
            ("configVal", configVal),
            ("configUpdate", configUpdate)
          ] <|>
    dir "static" (serveDirectory ".")

timesheet :: Snap ()
timesheet = do
    setTimeout 3600
    param <- getParam "tsparam"
    maybe (writeBS "must specify the month and year in URL, like so: /timesheet/2012-11")
          handleTimesheet param

handleTimesheet param = do
	jsonData <- liftIO $ Timesheet.process $ TE.decodeUtf8 param
	writeLBS jsonData

configdesc :: Snap ()
configdesc = writeLBS Timesheet.getEventProvidersConfig

configVal :: Snap ()
configVal = do
	settingsFile <- liftIO Config.getConfigFileName
	serveFile $ settingsFile

configUpdate :: Snap ()
configUpdate = do
	configFileName <- liftIO Config.getConfigFileName
	liftIO $ renameFile configFileName (configFileName ++ ".bak")
	outH <- liftIO $ openFile configFileName WriteMode
	requestBody <- readRequestBody 64000
	liftIO $ hPut outH requestBody
	liftIO $ hClose outH
