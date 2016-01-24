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

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {
        postBuild = doPostBuild
    }

doPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
doPostBuild _ _ pkg_descr lbi = do
    -- the following will only work on linux, don't check the return code as
    -- it's expected to fail on other OSses.
    rawSystem "xdg-icon-resource" ["install", "--size", "64", "cigale-timesheet-64.png", "cigale-timesheet"]
    rawSystem "xdg-icon-resource" ["install", "--size", "96", "cigale-timesheet-96.png", "cigale-timesheet"]
    rawSystem "xdg-desktop-menu" ["install", "cigale-timesheet.desktop"]
    return ()
