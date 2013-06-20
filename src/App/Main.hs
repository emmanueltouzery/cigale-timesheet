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
import qualified Language.Elm as Elm
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.String

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
            ("configUpdate", configUpdate),
            ("elm/:filename", serveElm)
          ] <|>
    dir "static" (serveDirectory ".")

timesheet :: Snap ()
timesheet = do
    modifyResponse $ setContentType "application/json"
    setTimeout 3600
    tsparam <- getParam "tsparam"
    maybe (writeBS "must specify the month and year in URL, like so: /timesheet/2012-11")
          handleTimesheet tsparam

serveElm :: Snap ()
serveElm = do
	paramFilename <- getParam "filename"
	let elmFilename = T.unpack $ TE.decodeUtf8 $ fromJust paramFilename
	let filename = "/home/emmanuel/Dropbox/haskell/timesheet/" ++ elmFilename ++ ".elm"
	--let filename = elmFilename ++ ".elm"
	elmSource <- liftIO $ readFile $ filename
	let elmJsLoc = "/static/elm-runtime.js"
	let compiled = docTypeHtml $ Elm.toHtml elmJsLoc "Timesheet" elmSource
	writeLBS $ DBLC.pack $ renderHtml compiled

handleTimesheet tsparam = do
	jsonData <- liftIO $ Timesheet.process $ TE.decodeUtf8 tsparam
	writeLBS jsonData

configdesc :: Snap ()
configdesc = do
	modifyResponse $ setContentType "application/json"
	writeLBS Timesheet.getEventProvidersConfig

configVal :: Snap ()
configVal = do
	modifyResponse $ setContentType "application/json"
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
