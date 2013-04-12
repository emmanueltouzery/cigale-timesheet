module Config where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import EventProvider

readConfig :: FromJSON a => [EventProvider a] -> IO [(EventProvider a, a)]
readConfig plugins = do
	input <- BL.readFile "src/config.json"
	let parsed = decode input :: Maybe Value
	case parsed of
		Nothing -> do
			printAndFail "config is NOT valid JSON"
		Just (Array vector) -> do
				putStrLn "config IS valid JSON"
				return $ parseConfigVector plugins vector
		_ -> do
			printAndFail "invalid config, must be an array of plugin options"

printAndFail :: String -> IO [(EventProvider a, a)]
printAndFail msg = do
	putStrLn msg
	error msg
	return []

parseConfigVector :: FromJSON a => [EventProvider a] -> Vector.Vector Value -> [(EventProvider a, a)]
parseConfigVector plugins configVector =
	Vector.toList $ fmap ((processConfigItem plugins) . configItemToPair) configVector
	where
		configItemToPair (Object hmap) = singleHashToPair hmap
		configItemToPair _ = error "configItemToPair expected an object"

singleHashToPair :: HashMap.HashMap k v -> (k, v)
singleHashToPair hmap =	if length list /= 1
				then error "hash doesn't contain 1 element"
				else head list
			where list = HashMap.toList hmap

processConfigItem :: FromJSON a => [EventProvider a] -> (T.Text, Value) -> (EventProvider a, a)
processConfigItem plugins (providername, config) =
	processConfigElement providersByName providername config
	where
		providersByName = HashMap.fromList $ map (\p -> (getModuleName p, p)) plugins

--processConfigElement :: FromJSON a => Map.Map String (EventProvider a) -> T.Text 
--				      -> Value -> (EventProvider a, a)
processConfigElement providersByName providerName configValue =
		(provider, (\(Success x) -> x) $ fromJSON configValue)
	where
		provider = providersByName HashMap.! T.unpack providerName

-- parseConfig :: ConfigSpec -> Value -> Config
-- parseConfig configSpec jsonValue = case jsonValue of
-- 	Object hashMap -> case configSpec of
-- 		SubElementArraySpec arrayspec ->
-- 			SubElementArray $ (parseConfig arrayspec) $ Map.assocs hashMap
-- 		_ -> error $ "unexpected object: " ++ (show hashMap)
-- 	Array vector -> case configSpec of
-- 		SubElementArraySpec arrayspec -> 
-- 			SubElementArray $ toList $ fmap (parseConfig arrayspec) vector
-- 		_ -> error $ "unexpected array: " ++ (show $ toList vector)
-- 	String text -> case configSpec of
-- 		StringFieldSpec title -> StringField text
-- 		_ -> error $ "unexpected text in json: " ++ text
-- 	Number number -> error "not accepting json integers"
-- 	Bool bool -> error "not accepting json booleans"
