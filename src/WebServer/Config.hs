{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables #-}

module Config where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.HashMap.Strict as Map hiding (map, filter, foldr)
import Data.Maybe
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Control.Applicative
import Control.Error
import Control.Monad.Trans (liftIO)

import EventProvider (EventProvider, getModuleName)
import qualified EventProviders
import qualified Util

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

getSettingsFolder :: IO FilePath
getSettingsFolder = do
	result <- (++ "/.cigale-timesheet/") <$> getHomeDirectory
	createDirectoryIfMissing False result
	return result

getConfigFileName :: IO FilePath
getConfigFileName = (++"config.json") <$> getSettingsFolder

readConfig :: (FromJSON a, ToJSON a) => [EventProvider a b] -> IO [(EventProvider a b, a)]
readConfig plugins = do
	settingsFile <- getConfigFileName
	parsed <- runMaybeT $ parseSettingsFile plugins settingsFile
	return $ fromMaybe [] parsed

parseSettingsFile :: (FromJSON a, ToJSON a) => [EventProvider a b] -> FilePath -> MaybeT IO [(EventProvider a b, a)]
parseSettingsFile plugins settingsFile = do
	fileContents <- hushT $ EitherT $ (Util.tryS $ BS.readFile settingsFile)
	(configMap :: HashMap String Array) <- hoistMaybe $ decodeStrict' fileContents
	let providersByNameHash = providersByName plugins
	hoistMaybe $ Util.concatMapM (processConfigItem providersByNameHash) (toList configMap)

processConfigItem :: (FromJSON a, ToJSON a) => HashMap String (EventProvider a b) -> (String, Array) -> Maybe [(EventProvider a b, a)]
processConfigItem providersByNameHash (providerName, config) = do
	provider <- Map.lookup providerName providersByNameHash
	return $ processConfigElement provider <$> Vector.toList config

providersByName :: (FromJSON a, ToJSON a) => [EventProvider a b] -> HashMap String (EventProvider a b)
providersByName plugins = HashMap.fromList $ map (\p -> (getModuleName p, p)) plugins

processConfigElement :: FromJSON a => EventProvider a b
				      -> Value -> (EventProvider a b, a)
processConfigElement provider configValue =
		(provider, (\(Success x) -> x) $ fromJSON configValue)

writeConfiguration :: (FromJSON a, ToJSON a) => [(EventProvider a b, a)] -> IO ()
writeConfiguration config = getConfigFileName >>= flip BL.writeFile jsonToWrite
	where jsonToWrite = encode $ groupByProvider config

groupByProvider :: [(EventProvider a b, a)] -> HashMap String [a]
groupByProvider = foldr mapAdd HashMap.empty
	where mapAdd (prov, cfg) = HashMap.insertWith (++) (getModuleName prov) [cfg]

addPluginInConfig :: BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
addPluginInConfig (T.unpack . TE.decodeUtf8 -> pluginName) configJson = runEitherT $ do
	newElt <- noteT ("invalid new config info " +++ configJson)
		$ hoistMaybe $ decodeIncomingConfigElt pluginName configJson
	liftIO $ (newElt:) <$> readConfig EventProviders.plugins >>= writeConfiguration
	return ""

decodeIncomingConfigElt :: String -> BS.ByteString -> Maybe (EventProvider Value Value, Value)
decodeIncomingConfigElt pluginName configJson = do
	let providersByNameHash = providersByName EventProviders.plugins
	provider <- Map.lookup pluginName providersByNameHash
	configValue <- decodeStrict' configJson
	return (provider, configValue)

deletePluginFromConfig :: BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
deletePluginFromConfig oldCfgItemStr (T.unpack . TE.decodeUtf8 -> pluginName) = runEitherT $ do
	config <- liftIO $ readConfig EventProviders.plugins
	configWithoutItem <- noteT ("Error removing") $
		hoistMaybe $ decodeStrict' oldCfgItemStr >>= checkRemoveFromConfig config pluginName
	liftIO $ writeConfiguration configWithoutItem
	return ""

updatePluginInConfig :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
updatePluginInConfig oldCfgItemStr (T.unpack . TE.decodeUtf8 -> pluginName) configJson = runEitherT $ do
	config <- liftIO $ readConfig EventProviders.plugins
	let errorMsg = "invalid new or old config info; new: [" +++ configJson
		+++ "], old: [" +++ oldCfgItemStr +++ "]"
	(newElt, configWithoutThisSource) <- noteT errorMsg $ hoistMaybe $ do
		nElt <- decodeIncomingConfigElt pluginName configJson
		oldConfigItem <- decodeStrict' oldCfgItemStr
		configWithoutElt <- checkRemoveFromConfig config pluginName oldConfigItem
		return (nElt, configWithoutElt)
	liftIO $ writeConfiguration (newElt:configWithoutThisSource)
	return ""

isConfigItem :: String -> HashMap T.Text Value -> (EventProvider Value b, Value) -> Bool
isConfigItem pluginName oldConfig (provider, config)
	| pluginName /= getModuleName provider = False
	| otherwise = allValuesMatch oldConfig config

-- Will return Nothing if removing from config failed,
-- otherwise will return Just newConfig.
checkRemoveFromConfig ::  [(EventProvider Value b, Value)] -> String -> HashMap T.Text Value -> Maybe [(EventProvider Value b, Value)]
checkRemoveFromConfig config pluginName oldConfigItem =
	let configWithoutThisSource = filter (not . isConfigItem pluginName oldConfigItem) config in
	if length configWithoutThisSource == length config
		then Nothing
		else Just configWithoutThisSource


allValuesMatch :: HashMap T.Text Value -> Value -> Bool
allValuesMatch clientVal (Object configVal) = clientVal == configVal
allValuesMatch _ _ = False
