{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards #-}

module Config where

import Reflex.Dom

import Data.Aeson as A
import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function

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

makeSimpleXhr :: (MonadWidget t m, FromJSON a) => String -> Event t b -> m (Dynamic t (RemoteData a))
makeSimpleXhr url postBuild = do
    req <- performRequestAsync $ const (xhrRequest "GET" url def) <$> postBuild
    holdDyn RemoteDataLoading $ fmap (readRemoteData . decodeXhrResponse) req

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    attrsDyn <- mapDyn (\curView -> styleWithHideIf (curView /= ActiveViewConfig) "height: 100%; padding-right: 10px;") activeViewDyn
    elDynAttr "div" attrsDyn $ do
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        postBuild <- getPostBuild
        cfgDescDyn <- makeSimpleXhr "/configdesc" postBuild
        cfgValDyn <- makeSimpleXhr "/configVal" postBuild
        readAllDyn <- combineDyn (liftA2 FetchedData) cfgDescDyn cfgValDyn
        void $ mapDyn displayConfig readAllDyn >>= dyn

displayConfig :: MonadWidget t m => RemoteData FetchedData -> m ()
displayConfig RemoteDataLoading = return ()
displayConfig RemoteDataInvalid = text "Error loading the server data!"
displayConfig (RemoteData (FetchedData configDesc configVal)) = do
    let cfgByProvider = buckets providerName configVal
    mapM_ (displayConfigSection configDesc) cfgByProvider

displayConfigSection :: MonadWidget t m => [PluginConfig] -> (String, [ConfigItem]) -> m ()
displayConfigSection pluginConfigs (secTitle, secItems) =
    void $ elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ text secTitle
        elAttr "div" ("class" =: "card-block") $
            elAttr "table" ("class" =: "table") $
                mapM displaySectionItem secItems

displaySectionItem :: MonadWidget t m => ConfigItem -> m ()
displaySectionItem ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        elAttr "div" ("class" =: "card-header") $
            text configItemName
        elAttr "div" ("class" =: "card-block") $
            text "TODO"

buckets :: Ord b => (a -> b) -> [a] -> [(b, [a])]
buckets f = map (\g -> (fst $ head g, map snd g))
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))
