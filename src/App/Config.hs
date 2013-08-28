{-# LANGUAGE OverloadedStrings #-}

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

import EventProvider
import qualified Settings (getSettingsFolder)

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
		Just configMap -> concatMap (processConfigItem plugins) (toList configMap)

processConfigItem :: FromJSON a => [EventProvider a] -> (String, Array) -> [(EventProvider a, a)]
processConfigItem plugins (providerName, config) =
		fmap (processConfigElement providersByName providerName) (Vector.toList config)
	where
		providersByName = HashMap.fromList $ map (\p -> (getModuleName p, p)) plugins

processConfigElement :: FromJSON a => HashMap String (EventProvider a) -> String 
				      -> Value -> (EventProvider a, a)
processConfigElement providersByName providerName configValue =
		(provider, (\(Success x) -> x) $ fromJSON configValue)
	where
		provider = fromJust $ HashMap.lookup providerName providersByName

addPluginInConfig :: BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString BS.ByteString)
addPluginInConfig pluginName configJson = do
	print pluginName
	return $ Right ""
