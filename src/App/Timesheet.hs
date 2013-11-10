{-# LANGUAGE OverloadedStrings, ExistentialQuantification, TemplateHaskell #-}
module Timesheet where

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Read
import Data.List
import Data.Time
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH (deriveJSON, mkToJSON, defaultOptions)

import GHC.Exts (sortWith)

import Control.Concurrent.Async

import qualified Util
import qualified Config
import qualified EventProviders
import EventProvider
import Event
import qualified FayAeson

process :: T.Text -> IO BL.ByteString
process monthStr = do
	config <- Config.readConfig EventProviders.plugins
	processConfig monthStr config

processConfig :: T.Text -> [(EventProvider Value, Value)] -> IO BL.ByteString
processConfig monthStr config = do
	-- TODO this will fail with cryptic error messages if not given
	-- a string by the right format!
	myTz <- getCurrentTimeZone
	let ymd = map (Util.safePromise . decimal) (T.splitOn "-" monthStr)
	let date = fromGregorian (toInteger $ head ymd) (ymd !! 1) (ymd !! 2)

	putStrLn "before the fetching..."
	print config
	putStrLn "after printing"
	settings <- getGlobalSettings 
	allEventsSeq <- sequence $ map (uncurry $ fetchProvider settings date) config
	let allEvents = foldl (++) [] allEventsSeq
	let sortedEvents = sortWith Event.eventDate allEvents
	--let noNullSortedEvents = map (\e -> e {
	--	fullContents = Just $ maybe "" id (fullContents e),
	--	}) sortedEvents
	let eventDates = fmap Event.eventDate sortedEvents
	let eventDatesLocal = fmap (utcToLocalTime myTz) eventDates
	putStrLn "after the fetching!"
	-- well would be faster to just check the first and last
	-- element... but it's actually shorter to code like this..
	let outOfRangeData = filter (outOfRange date (addDays 1 date)) eventDatesLocal
	let ok = null outOfRangeData
	if ok
		then return $ JSON.encode sortedEvents --noNullSortedEvents
		else do
			putStrLn "*** SOME EVENTS ARE NOT IN TIME RANGE"
			print outOfRangeData
			print $ head sortedEvents
			print $ last sortedEvents
			return BL.empty
	where
		outOfRange start end time = time < (LocalTime start midnight) || time > (LocalTime end midnight)

getGlobalSettings :: IO GlobalSettings
getGlobalSettings = do
	settingsFolder <- Config.getSettingsFolder
	return $ GlobalSettings { getSettingsFolder = settingsFolder }

fetchProvider :: GlobalSettings -> Day -> EventProvider Value -> Value -> IO [Event]
fetchProvider settings day provider config = do
	putStrLn $ "fetching from " ++ (getModuleName provider)
	print config
	events <- getEvents provider config settings day
	putStrLn $ "found " ++ (show $ length events) ++ " events."
	return events

data PluginConfig = PluginConfig
	{
		cfgPluginName :: String,
		cfgPluginConfig :: [ConfigDataInfo]
	}
instance ToJSON PluginConfig where
    toJSON = (FayAeson.addInstance "PluginConfig") . $(mkToJSON defaultOptions ''PluginConfig)

getEventProvidersConfig :: BL.ByteString
getEventProvidersConfig = JSON.encode $ fmap getPluginConfig EventProviders.plugins

getPluginConfig :: EventProvider a -> PluginConfig
getPluginConfig plugin = PluginConfig
	{
		cfgPluginName = getModuleName plugin,
		cfgPluginConfig = getConfigType plugin
	}
