{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Config where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.HashMap.Strict as Map hiding (map)
import Data.Maybe
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import EventProvider
import qualified Settings (getSettingsFolder)
import qualified EventProviders

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.json") Settings.getSettingsFolder

readConfig :: FromJSON a => [EventProvider a] -> IO [(EventProvider a, a)]
readConfig plugins = do
	settingsFile <- getConfigFileName
	isSettings <- doesFileExist settingsFile
	if isSettings
		then BL.readFile settingsFile >>= (return . parseSettingsFile plugins)
		else return []

parseSettingsFile :: FromJSON a => [EventProvider a] -> BL.ByteString -> [(EventProvider a, a)]
parseSettingsFile plugins input = let parsed = decode input :: Maybe (HashMap String Array) in
	case parsed of
		Nothing -> error "config is NOT valid JSON"
		Just configMap -> concatMap (processConfigItem providersByNameHash) (toList configMap)
	where
		providersByNameHash = providersByName plugins

processConfigItem :: FromJSON a => HashMap String (EventProvider a) -> (String, Array) -> [(EventProvider a, a)]
processConfigItem providersByNameHash (providerName, config) =
		fmap (processConfigElement provider) (Vector.toList config)
		where
			provider = fromJust $ HashMap.lookup providerName providersByNameHash

providersByName :: FromJSON a => [EventProvider a] -> HashMap String (EventProvider a)
providersByName plugins = HashMap.fromList $ map (\p -> (getModuleName p, p)) plugins

processConfigElement :: FromJSON a => EventProvider a
				      -> Value -> (EventProvider a, a)
processConfigElement provider configValue =
		(provider, (\(Success x) -> x) $ fromJSON configValue)

addPluginInConfig :: BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
addPluginInConfig (T.unpack . TE.decodeUtf8 -> pluginName) configJson = do
	let providersByNameHash = providersByName EventProviders.plugins
	let provider = fromJust $ HashMap.lookup pluginName providersByNameHash
	print pluginName
	return $ Right ""
