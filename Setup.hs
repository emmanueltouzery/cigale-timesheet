#! /usr/bin/env runhaskell --user -XOverloadedStrings
{-# LANGUAGE OverloadedStrings #-}

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup (BuildFlags)

import System.Exit
import Data.Monoid
import System.Directory
import qualified Data.ByteString.Lazy as LBS (readFile)
import System.Process (runCommand, waitForProcess, rawSystem)

-- probably message that you need zip-archive and fay and fay-jquery
-- before attempting to build...

import qualified Codec.Archive.Zip as Zip

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {
        postBuild = doPostBuild
    }

doPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
doPostBuild _ _ pkg_descr lbi = do
    let appDataDir = datadir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
    putStrLn appDataDir
    createDirectoryIfMissing True appDataDir
    unzipToTarget "lib/bootstrap-3.0.0-dist.zip" appDataDir
    unzipToTarget "lib/jquery-ui-1.9.2.custom.zip" appDataDir
    copyFile "lib/jquery-2.0.3.min.js" (appDataDir ++ "/jquery-2.0.3.min.js")
    copyFile "lib/knockout-2.3.0.js" (appDataDir ++ "/knockout-2.3.0.js")
    copyFile "src/WebClient/FayApp.html" (appDataDir ++ "/FayApp.html")
    copyFile "src/WebClient/FilePickerModal.html"  (appDataDir ++ "/FilePickerModal.html")
    copyFile "src/WebClient/FayConfig.html" (appDataDir ++ "/FayConfig.html")
    -- the following will only work on linux, don't check the return code as
    -- it's expected to fail on other OSses.
    rawSystem "xdg-icon-resource" ["install", "--size", "64", "cigale-timesheet-64.png", "cigale-timesheet"]
    rawSystem "xdg-icon-resource" ["install", "--size", "96", "cigale-timesheet-96.png", "cigale-timesheet"]
    rawSystem "xdg-desktop-menu" ["install", "cigale-timesheet.desktop"]
    return ()

unzipToTarget :: FilePath -> String -> IO ()
unzipToTarget sourceFile targetFolder = do
    zipContents <- LBS.readFile sourceFile
    Zip.extractFilesFromArchive [Zip.OptDestination targetFolder] (Zip.toArchive zipContents)
