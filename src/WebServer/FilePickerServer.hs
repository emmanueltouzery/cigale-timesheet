{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module FilePickerServer where

import System.IO (withFile, IOMode(ReadMode), hFileSize)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Aeson (encode, ToJSON)
import GHC.Generics
import Snap.Core
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)

import Util (toStrict1)
import SnapUtil (getSingleParam, setResponse)

data BrowseResponse = BrowseResponse
	{
		browseFolderPath :: String,
		browseFiles :: [FileInfo]
	} deriving (Show, Generic)
instance ToJSON BrowseResponse

data FileInfo = FileInfo
	{
		filename :: String,
		-- filesize will be -1 for a directory
		filesize :: Integer
	} deriving (Show, Generic)
instance ToJSON FileInfo

browseFolder :: Snap ()
browseFolder = do
	modifyResponse $ setContentType "application/json"
	curFolder <- liftM (TE.decodeUtf8 . (fromMaybe "")) (getSingleParam "path")
	files <- liftIO $ getDirectoryContents "/home/emmanuel"
	fileInfos <- liftIO $ mapM (fileInfo "/home/emmanuel") files
	let response = BrowseResponse "/home/emmanuel" fileInfos
	setResponse $ Right $ toStrict1 $ encode response

fileInfo :: String -> String -> IO FileInfo
fileInfo folder fName = do
	let fullPath = folder </> fName
	isFile <- doesFileExist fullPath
	fSize <- if isFile
		then withFile fullPath ReadMode hFileSize
		else return (-1)
	return FileInfo { filename = fName, filesize = fSize}
