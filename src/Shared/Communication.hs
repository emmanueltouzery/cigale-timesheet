{-# LANGUAGE DeriveGeneric #-}
module Communication where

import TsEvent
import EventProvider
import Data.Aeson
import GHC.Generics

data FetchResponse = FetchResponse
    {
        fetchedEvents :: [TsEvent],
        fetchErrors   :: [String]
    } deriving (Show, Generic)
instance ToJSON   FetchResponse
instance FromJSON FetchResponse

data PluginConfig = PluginConfig
    {
        cfgPluginName   :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    } deriving (Eq, Show, Generic)
instance ToJSON   PluginConfig
instance FromJSON PluginConfig

data FileInfo = FileInfo
    {
        filename :: String,
        -- filesize will be -1 for a directory
        filesize :: Integer
    } deriving (Show, Generic)
instance ToJSON   FileInfo
instance FromJSON FileInfo

data BrowseResponse = BrowseResponse
    {
        browseFolderPath :: String,
        browseFiles :: [FileInfo]
    } deriving (Show, Generic)
instance ToJSON   BrowseResponse
instance FromJSON BrowseResponse
