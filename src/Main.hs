{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as TE

import qualified Timesheet

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "enter in the URL timesheet/ the month for which to display data, for instance timesheet/2012-11") <|>
    route [ ("timesheet/:tsparam", timesheet)
          ] <|>
    dir "static" (serveDirectory ".")

timesheet :: Snap ()
timesheet = do
    param <- getParam "tsparam"
    maybe (writeBS "must specify the month and year in URL, like so: /timesheet/2012-11")
          handleTimesheet param

handleTimesheet param = do
	jsonData <- liftIO $ Timesheet.process $ TE.decodeUtf8 param
	writeLBS jsonData

