{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}

module Config where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict as Map hiding (map, filter, foldr)
import Data.Maybe
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Control.Error
import Control.Monad.Trans (liftIO)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.List
import Data.Monoid

import EventProvider (EventProvider, getModuleName)
import qualified EventProviders
import qualified Util
import SnapUtil (noteET)

data ConfigItem = ConfigItem
    {
        configItemName :: Text,
        providerName   :: Text,
        configuration  :: Value
    } deriving Show
deriveJSON defaultOptions ''ConfigItem

data EventSource a b = EventSource
    {
        srcName :: T.Text,
        srcProvider :: EventProvider a b,
        srcConfig :: a
    } deriving Show

getSettingsFolder :: IO FilePath
getSettingsFolder = do
    result <- (++ "/.cigale-timesheet/") <$> getHomeDirectory
    createDirectoryIfMissing False result
    return result

getConfigFileName :: IO FilePath
getConfigFileName = (++"config.json") <$> getSettingsFolder

readConfig :: (FromJSON a, ToJSON a, FromJSON b) => [EventProvider a b] -> IO [EventSource a b]
readConfig plugins = do
    settingsFile <- getConfigFileName
    parsed <- runMaybeT $ parseSettingsFile plugins settingsFile
    return $ fromMaybe [] parsed

parseSettingsFile :: (FromJSON a, ToJSON a, FromJSON b) =>
    [EventProvider a b] -> FilePath -> MaybeT IO [EventSource a b]
parseSettingsFile plugins settingsFile = do
    fileContents <- hushT $ ExceptT $ Util.tryS (BS.readFile settingsFile)
    configItems <- hoistMaybe $ decodeStrict' fileContents
    let providersByNameHash = providersByName plugins
    hoistMaybe $ mapM (processConfigItem providersByNameHash) configItems

processConfigItem :: (FromJSON a, ToJSON a) =>
    HashMap T.Text (EventProvider a b) -> ConfigItem -> Maybe (EventSource a b)
processConfigItem providersByNameHash configItem = do
    provider <- Map.lookup (providerName configItem) providersByNameHash
    EventSource (configItemName configItem) provider <$> fromJsonM (configuration configItem)

fromJsonM :: FromJSON a => Value -> Maybe a
fromJsonM = resultToMaybe . fromJSON

resultToMaybe :: FromJSON a => Result a -> Maybe a
resultToMaybe (Success x) = Just x
resultToMaybe _ = Nothing

providersByName :: (FromJSON a, ToJSON a) => [EventProvider a b] -> HashMap T.Text (EventProvider a b)
providersByName plugins = HashMap.fromList $ map (\p -> (T.pack $ getModuleName p, p)) plugins

writeConfiguration :: (FromJSON a, ToJSON a) => [EventSource a b] -> IO ()
writeConfiguration eventSources = getConfigFileName >>= flip BL.writeFile jsonToWrite
    where
        jsonToWrite = encode $ toConfigItem <$> eventSources
        toConfigItem (EventSource n p c) = ConfigItem n (T.pack $ getModuleName p) (toJSON c)

deletePluginFromConfig :: BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
deletePluginFromConfig (TE.decodeUtf8 -> cfgItemName) = runExceptT $ do
    config <- liftIO $ readConfig EventProviders.plugins
    configWithoutItem <- noteET "Error removing"
        $ checkRemoveFromConfig config cfgItemName
    liftIO $ writeConfiguration configWithoutItem
    return ""

-- Will return Nothing if removing from config failed,
-- otherwise will return Just newConfig.
checkRemoveFromConfig ::  [EventSource a b] -> T.Text -> Maybe [EventSource a b]
checkRemoveFromConfig config cfgItemName =
    let configWithoutThisSource = filter ((/=cfgItemName) . srcName) config in
    if length configWithoutThisSource == length config
        then Nothing
        else Just configWithoutThisSource

updatePluginInConfig :: T.Text -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
updatePluginInConfig oldConfigItemName configItemJson = runExceptT $ do
    config <- liftIO $ readConfig EventProviders.plugins
    let errorMsg = "invalid new config info: " <> configItemJson
    (configItem, configWithoutThisSource) <- noteET errorMsg $ do
        nElt <- decodeStrict' configItemJson
        configWithoutElt <- checkRemoveFromConfig config oldConfigItemName
        return (nElt, configWithoutElt)
    ensureUniqueEventSourceName (configItemName configItem) configWithoutThisSource
    let providersByNameHash = providersByName EventProviders.plugins
    newElt <- noteET "Error reading new config item"
        $ processConfigItem providersByNameHash configItem
    liftIO $ writeConfiguration (newElt:configWithoutThisSource)
    return ""

addPluginInConfig :: BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
addPluginInConfig configItemJson = runExceptT $ do
    configItem <- noteET ("invalid new config info " <> configItemJson)
        $ decodeStrict' configItemJson
    let providersByNameHash = providersByName EventProviders.plugins
    newElt <- noteET "Error reading new config item"
        $ processConfigItem providersByNameHash configItem
    config <- liftIO $ readConfig EventProviders.plugins
    ensureUniqueEventSourceName (configItemName configItem) config
    liftIO $ writeConfiguration (newElt:config)
    return ""

ensureUniqueEventSourceName :: T.Text -> [EventSource a b] -> ExceptT BS.ByteString IO BS.ByteString
ensureUniqueEventSourceName name events = hoistEither $ case find ((==name) . srcName) events of
    Just _ -> Left "Duplicate config name"
    Nothing -> Right "OK"
