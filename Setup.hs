#! /usr/bin/env runhaskell --user

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags, InstallFlags, BuildFlags)
import Distribution.Simple.InstallDirs

import System.Process
import System.Exit
import System.Directory

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
	exit1 <- compileFay "src/App2/" "FayApp.hs" "FayApp.js" appDataDir
	exit2 <- compileFay "src/App2/" "FayConfig.hs" "FayConfig.js" appDataDir
	print exit1

compileFay :: String -> String -> String -> FilePath -> IO ExitCode
compileFay sourceFolder filename targetFilename appDataDir =
	(runCommand $ "fay --package fay-jquery --include " ++ sourceFolder ++ " "
		++ sourceFolder ++ filename ++ " -o "
		++ appDataDir ++ "/" ++ targetFilename) >>= waitForProcess
