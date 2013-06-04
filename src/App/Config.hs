module Config where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.HashMap.Strict as Map hiding (map)
import Data.Maybe

import EventProvider
import qualified Settings (getSettingsFolder)

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.json") Settings.getSettingsFolder 

readConfig :: FromJSON a => [EventProvider a] -> IO [(EventProvider a, a)]
readConfig plugins = do
	settingsFile <- getConfigFileName
	input <- BL.readFile $ settingsFile
	let parsed = decode input :: Maybe (HashMap String Array)
	case parsed of
		Nothing -> do
			printAndFail "config is NOT valid JSON"
		Just configMap -> do
				putStrLn "config IS valid JSON"
				return $ concatMap (processConfigItem plugins) (toList configMap)

printAndFail :: String -> IO [(EventProvider a, a)]
printAndFail msg = do
	putStrLn msg
	error msg
	return []

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
