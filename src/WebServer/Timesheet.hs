{-# LANGUAGE OverloadedStrings, ExistentialQuantification, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Timesheet where

import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Time
import Data.Aeson
import Data.Aeson.TH (mkToJSON, defaultOptions)
import Control.Error hiding (err)
import Data.List.Utils (mergeBy)
import Data.Function (on)

import qualified Config
import qualified EventProviders
import EventProvider
import Event
import qualified FayAeson

process :: Day -> IO BL.ByteString
process month = do
	config <- Config.readConfig EventProviders.plugins
	processConfig month config

data FetchResponse = FetchResponse
	{
		fetchedEvents :: [Event],
		fetchErrors :: [String]
	}
instance ToJSON FetchResponse where
     toJSON = FayAeson.addInstance "FetchResponse" . $(mkToJSON defaultOptions ''FetchResponse)

fetchResponseAdd :: FetchResponse -> Either String [Event] -> FetchResponse
fetchResponseAdd sofar@(FetchResponse _ errs) (Left err) = sofar { fetchErrors = err:errs }
fetchResponseAdd sofar@(FetchResponse ev _) (Right evts) =
	sofar { fetchedEvents = mergeBy (compare `on` Event.eventDate) ev evts }

processConfig :: Day -> [(EventProvider Value b, Value)] -> IO BL.ByteString
processConfig date config = do
	myTz <- getTimeZone $ UTCTime date (secondsToDiffTime 8*3600)

	putStrLn "before the fetching..."
	settings <- getGlobalSettings 
	allEventsSeq <- mapM (uncurry $ fetchProvider settings date) config
	let allEvents = foldl' fetchResponseAdd (FetchResponse [] []) allEventsSeq
	let eventDates = Event.eventDate <$> fetchedEvents allEvents
	let eventDatesLocal = fmap (utcToLocalTime myTz) eventDates
	putStrLn "after the fetching!"
	-- well would be faster to just check the first and last
	-- element... but it's actually shorter to code like this..
	let outOfRangeData = filter (outOfRange date (addDays 1 date)) eventDatesLocal
	if null outOfRangeData
		then return $ JSON.encode allEvents
		else do
			putStrLn "*** SOME EVENTS ARE NOT IN TIME RANGE"
			print outOfRangeData
			print $ head $ fetchedEvents allEvents
			print $ last $ fetchedEvents allEvents
			return BL.empty
	where
		outOfRange start end time = time < LocalTime start midnight || time > LocalTime end midnight

getGlobalSettings :: IO GlobalSettings
getGlobalSettings = do
	settingsFolder <- Config.getSettingsFolder
	return GlobalSettings { getSettingsFolder = settingsFolder }

fetchProvider :: GlobalSettings -> Day -> EventProvider Value b -> Value -> IO (Either String [Event])
fetchProvider settings day provider config = do
	putStrLn $ "fetching from " ++ getModuleName provider
	evts <- runEitherT $ getEvents provider config settings day
	putStrLn "Done"
	return evts

data PluginConfig = PluginConfig
	{
		cfgPluginName :: String,
		cfgPluginConfig :: [ConfigDataInfo]
	}
instance ToJSON PluginConfig where
    toJSON = FayAeson.addInstance "PluginConfig" . $(mkToJSON defaultOptions ''PluginConfig)

getEventProvidersConfig :: BL.ByteString
getEventProvidersConfig = JSON.encode $ fmap getPluginConfig EventProviders.plugins

getPluginConfig :: EventProvider a b -> PluginConfig
getPluginConfig plugin = PluginConfig
	{
		cfgPluginName = getModuleName plugin,
		cfgPluginConfig = getConfigType plugin
	}
