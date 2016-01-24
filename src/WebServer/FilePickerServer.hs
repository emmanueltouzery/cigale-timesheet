{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module FilePickerServer where

import System.IO (withFile, IOMode(ReadMode), hFileSize)
import System.Directory (doesFileExist, getDirectoryContents, getHomeDirectory)
import System.FilePath ((</>))
import GHC.Generics
import Snap.Core
import Data.Maybe (fromMaybe)
import Control.Exception (catch, SomeException)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Aeson (encode, ToJSON, ToJSON(..))

data FileInfo = FileInfo
    {
        filename :: String,
        -- filesize will be -1 for a directory
        filesize :: Integer
    } deriving (Show, Generic)
instance ToJSON FileInfo

data BrowseResponse = BrowseResponse
    {
        browseFolderPath :: String,
        browseFiles :: [FileInfo]
    } deriving (Show, Generic)
instance ToJSON BrowseResponse

browseFolder :: Snap ()
browseFolder = do
    homeDir <- liftIO $ TE.encodeUtf8 . T.pack <$> getHomeDirectory
    modifyResponse $ setContentType "application/json"
    curFolder <- T.unpack . TE.decodeUtf8 . fromMaybe homeDir <$> getParam "path"
    files <- liftIO $ getDirectoryContents curFolder
    fileInfos <- liftIO $ mapM (fileInfo curFolder) files
    let response = BrowseResponse curFolder fileInfos
    writeLBS $ encode response

fileInfo :: String -> String -> IO FileInfo
fileInfo folder fName = do
    let fullPath = folder </> fName
    isFile <- doesFileExist fullPath
    fSize <- if isFile
        then catch (withFile fullPath ReadMode hFileSize)
            (\(_ :: SomeException) -> return (-2))
        else return (-1)
    return FileInfo { filename = fName, filesize = fSize}
