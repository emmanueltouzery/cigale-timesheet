{-# LANGUAGE DeriveGeneric #-}
module Communication where

import Event
import EventProvider
import Data.Aeson
import GHC.Generics

data FetchResponse = FetchResponse
    {
        fetchedEvents :: [Event],
        fetchErrors :: [String]
    } deriving Generic
instance ToJSON FetchResponse

data PluginConfig = PluginConfig
    {
        cfgPluginName :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    } deriving Generic
instance ToJSON PluginConfig
