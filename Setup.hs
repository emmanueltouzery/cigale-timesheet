#! /usr/bin/env runhaskell --user

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags, InstallFlags, BuildFlags)
import Distribution.Simple.InstallDirs

import System.IO
import System.Process
import System.Exit
import System.Directory
import Data.ByteString.Lazy as LBS (readFile)
import qualified Data.ByteString.Lazy as B
import Control.Monad ( when, unless, zipWithM )
import System.FilePath.Posix

import Codec.Archive.Zip as Zip

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
	unzipToTarget "lib/bootstrap-3.0.0.zip" appDataDir
	unzipToTarget "lib/jquery-ui-1.9.2.zip" appDataDir

compileFay :: String -> String -> String -> FilePath -> IO ExitCode
compileFay sourceFolder filename targetFilename appDataDir =
	(runCommand $ "fay --package fay-jquery --include " ++ sourceFolder ++ " "
		++ sourceFolder ++ filename ++ " -o "
		++ appDataDir ++ "/" ++ targetFilename) >>= waitForProcess

unzipToTarget :: FilePath -> String -> IO ()
unzipToTarget sourceFile targetFolder = do
	zipContents <- LBS.readFile sourceFile
	myExtractFilesFromArchive [] targetFolder (toArchive zipContents)

-- | Extract all files from an 'Archive', creating directories
-- as needed.  If 'OptVerbose' is specified, print messages to stderr.
-- Note that the last-modified time is set correctly only in POSIX,
-- not in Windows.
myExtractFilesFromArchive :: [ZipOption] -> FilePath -> Archive -> IO ()
myExtractFilesFromArchive opts targetFolder archive = mapM_ (myWriteEntry opts targetFolder) $ zEntries archive

-- | Writes contents of an 'Entry' to a file.
myWriteEntry :: [ZipOption] -> FilePath -> Entry -> IO ()
myWriteEntry opts targetFolder entry = do
  let path = targetFolder ++ "/" ++ eRelativePath entry
  -- create directories if needed
  let dir = takeDirectory path
  exists <- doesDirectoryExist dir
  unless exists $ do
    createDirectoryIfMissing True dir
    when (OptVerbose `elem` opts) $
      hPutStrLn stderr $ "  creating: " ++ dir
  if length path > 0 && last path == '/' -- path is a directory
     then return ()
     else do
       when (OptVerbose `elem` opts) $ do
         hPutStrLn stderr $ case eCompressionMethod entry of
                                 Deflate       -> " inflating: " ++ path
                                 NoCompression -> "extracting: " ++ path
       B.writeFile path (fromEntry entry)
  -- Note that last modified times are supported only for POSIX, not for
  -- Windows.
  --setFileTimeStamp path (eLastModified entry)
