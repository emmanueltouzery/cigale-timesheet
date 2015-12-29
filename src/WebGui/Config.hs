{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings #-}

module Config where

import Reflex.Dom

import Data.Aeson as A
import GHC.Generics
import Control.Applicative

import Common

-- TODO stop copy-pasting this between client & server

data PluginConfig = PluginConfig
    {
        cfgPluginName :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    } deriving Generic
instance FromJSON PluginConfig

data ConfigDataInfo = ConfigDataInfo
    {
        memberName :: String,
        memberType :: String
    } deriving (Eq, Show, Generic)
instance FromJSON ConfigDataInfo

data ConfigItem = ConfigItem
    {
        configItemName :: String,
        providerName :: String,
        configuration :: A.Value
    } deriving (Show, Generic)
instance FromJSON ConfigItem

data FetchedData = FetchedData
    {
        fetchedConfigDesc :: [PluginConfig],
        fetchedConfigVal  :: [ConfigItem]
    }

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    attrsDyn <- mapDyn (\curView -> styleWithHideIf (curView /= ActiveViewConfig) "height: 100%;") activeViewDyn
    elDynAttr "div" attrsDyn $ do
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        postBuild <- getPostBuild
        cfgDescReq <- performRequestAsync $ const (xhrRequest "GET" "/configdesc" def) <$> postBuild -- [PluginConfig]
        cfgDescDyn <- holdDyn RemoteDataLoading $ fmap (readRemoteData . decodeXhrResponse) cfgDescReq
        cfgValReq <- performRequestAsync $ const (xhrRequest "GET" "/configVal" def) <$> postBuild -- [ConfigItem]
        cfgValDyn <- holdDyn RemoteDataLoading $ fmap (readRemoteData . decodeXhrResponse) cfgValReq
        readAllDyn <- combineDyn (liftA2 FetchedData) cfgDescDyn cfgValDyn
        mapDyn displayConfig readAllDyn >>= dyn
        return ()

displayConfig :: MonadWidget t m => RemoteData FetchedData -> m ()
displayConfig RemoteDataLoading = return ()
displayConfig RemoteDataInvalid = text "Error loading the server data!"
displayConfig (RemoteData (FetchedData configDesc configVal)) = text "Got it!"
