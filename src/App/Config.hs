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
import qualified Util

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.json") Settings.getSettingsFolder

readConfig :: (FromJSON a, ToJSON a) => [EventProvider a] -> IO [(EventProvider a, a)]
readConfig plugins = do
	settingsFile <- getConfigFileName
	isSettings <- doesFileExist settingsFile
	if isSettings
		then BS.readFile settingsFile >>= (return . parseSettingsFile plugins)
		else return []

parseSettingsFile :: (FromJSON a, ToJSON a) => [EventProvider a] -> BS.ByteString -> [(EventProvider a, a)]
parseSettingsFile plugins input = let parsed = Util.decodeStrict input :: Maybe (HashMap String Array) in
	case parsed of
		Nothing -> error "config is NOT valid JSON"
		Just configMap -> concatMap (processConfigItem providersByNameHash) (toList configMap)
	where
		providersByNameHash = providersByName plugins

processConfigItem :: (FromJSON a, ToJSON a) => HashMap String (EventProvider a) -> (String, Array) -> [(EventProvider a, a)]
processConfigItem providersByNameHash (providerName, config) =
		fmap (processConfigElement provider) (Vector.toList config)
		where
			provider = fromJust $ HashMap.lookup providerName providersByNameHash

providersByName :: (FromJSON a, ToJSON a) => [EventProvider a] -> HashMap String (EventProvider a)
providersByName plugins = HashMap.fromList $ map (\p -> (getModuleName p, p)) plugins

processConfigElement :: FromJSON a => EventProvider a
				      -> Value -> (EventProvider a, a)
processConfigElement provider configValue =
		(provider, (\(Success x) -> x) $ fromJSON configValue)

writeConfiguration :: (FromJSON a, ToJSON a) => [(EventProvider a, a)] -> IO ()
writeConfiguration config = getConfigFileName >>= (flip BL.writeFile) jsonToWrite
		where
			jsonToWrite = encode $ groupByProvider config HashMap.empty

groupByProvider :: [(EventProvider a, a)] -> HashMap String [a] -> HashMap String [a]
groupByProvider [] result = result
groupByProvider ((plugin, config):xs) result = groupByProvider xs (case HashMap.lookup moduleName result of
						Nothing -> HashMap.insert moduleName [config] result
						Just array -> HashMap.insert moduleName (config:array) result)
		where
			moduleName = getModuleName plugin

addPluginInConfig :: BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
addPluginInConfig (T.unpack . TE.decodeUtf8 -> pluginName) configJson = do
	let providersByNameHash = providersByName EventProviders.plugins
	let provider = fromJust $ HashMap.lookup pluginName providersByNameHash
	print pluginName
	case Util.decodeStrict configJson of
		Nothing -> return $ Left "invalid json"
		Just configValue -> do
			let newElt = (provider, configValue)
			readConfig EventProviders.plugins >>= (return . \x -> newElt:x) >>= writeConfiguration
			return $ Right ""
