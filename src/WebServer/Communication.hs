module Communication where

import Event
import EventProvider

data FetchResponse = FetchResponse
    {
        fetchedEvents :: [Event],
        fetchErrors :: [String]
    }

data PluginConfig = PluginConfig
    {
        cfgPluginName :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    }
