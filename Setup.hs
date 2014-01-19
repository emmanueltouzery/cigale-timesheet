#! /usr/bin/env runhaskell --user -XOverloadedStrings
{-# LANGUAGE OverloadedStrings #-}

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags, InstallFlags, BuildFlags)
import Distribution.Simple.InstallDirs

import System.IO
import System.Exit
import System.Directory
import qualified Data.ByteString.Lazy as LBS (readFile)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.Text.Encoding as T
import System.Process (runCommand, waitForProcess, rawSystem)
import qualified Data.Text as T
import Control.Monad (liftM)
import System.FilePath

-- probably message that you need zip-archive and fay and fay-jquery
-- before attempting to build...

import qualified Codec.Archive.Zip as Zip

main = defaultMainWithHooks simpleUserHooks
	{
		postBuild = doPostBuild
	}

-- i found it difficult to find information about this...
-- helped myself also with this code: https://github.com/Eelis/geordi/blob/master/Setup.hs
doPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
doPostBuild _ _ pkg_descr lbi = do
	let idt = installDirTemplates lbi
	let env = installDirsTemplateEnv idt
	let idt' = fmap (fromPathTemplate
	      . substPathTemplate env
	      . substPathTemplate (packageTemplateEnv (package pkg_descr))) idt
	let appDataDir = datadir idt' ++ "/" ++ datasubdir idt' 
	putStrLn appDataDir
	createDirectoryIfMissing True appDataDir
	compileFay "src/WebClient/" "FayApp.hs" "FayApp.js" appDataDir
	compileFay "src/WebClient/" "FayConfig.hs" "FayConfig.js" appDataDir
	unzipToTarget "lib/bootstrap-3.0.0-dist.zip" appDataDir
	unzipToTarget "lib/jquery-ui-1.9.2.custom.zip" appDataDir
	copyFile "lib/jquery-2.0.3.min.js" (appDataDir ++ "/jquery-2.0.3.min.js")
	copyFile "lib/knockout-2.3.0.js" (appDataDir ++ "/knockout-2.3.0.js")
	copyFile "src/WebClient/FayApp.html" (appDataDir ++ "/FayApp.html")
	copyFile "src/WebClient/FayConfig.html" (appDataDir ++ "/FayConfig.html")
	-- the following will only work on linux, don't check the return code as
	-- it's expected to fail on other OSses.
	rawSystem "xdg-icon-resource" ["install", "--size", "64", "cigale-timesheet-64.png", "cigale-timesheet"]
	rawSystem "xdg-icon-resource" ["install", "--size", "96", "cigale-timesheet-96.png", "cigale-timesheet"]
	rawSystem "xdg-desktop-menu" ["install", "cigale-timesheet.desktop"]
	return ()

compileFay :: String -> String -> String -> FilePath -> IO ()
compileFay sourceFolder filename targetFilename appDataDir = do
	let command = "fay --package fay-jquery,fay-text --include " ++ sourceFolder ++ " "
		++ sourceFolder ++ filename ++ " -o "
		++ appDataDir ++ "/" ++ targetFilename ++ " --pretty"
	putStrLn command
	exitCode <- runCommand command >>= waitForProcess
	case exitCode of
		ExitSuccess -> return ()
		ExitFailure _ -> exitWith exitCode

unzipToTarget :: FilePath -> String -> IO ()
unzipToTarget sourceFile targetFolder = do
	zipContents <- LBS.readFile sourceFile
	Zip.extractFilesFromArchive [Zip.OptDestination targetFolder] (Zip.toArchive zipContents)
