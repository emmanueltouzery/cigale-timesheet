{-# LANGUAGE OverloadedStrings, ExistentialQuantification, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Timesheet where

import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS
import Data.List
import Data.Time
import Data.Aeson
import Data.Aeson.TH (mkToJSON, defaultOptions)
import Control.Error hiding (err)
import Data.List.Utils (mergeBy)
import Data.Function (on)
import Control.Applicative
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlEncode)
import qualified Data.Aeson as Aeson
import Data.Char (chr)

import qualified Config
import qualified EventProviders
import EventProvider
import Event
import qualified FayAeson

process :: Day -> IO (Bool, BL.ByteString)
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
	sofar { fetchedEvents = mergeBy orderByDate ev (sortBy orderByDate evts) }
	where orderByDate = compare `on` Event.eventDate

processConfig :: Day -> [Config.EventSource Value Value] -> IO (Bool, BL.ByteString)
processConfig date config = do
	myTz <- getTimeZone $ UTCTime date (secondsToDiffTime 8*3600)

	putStrLn "before the fetching..."
	settings <- getGlobalSettings 
	allEventsSeq <- mapM (\c -> fetchProvider (Config.srcName c) settings date c) config
	let allEvents = foldl' fetchResponseAdd (FetchResponse [] []) allEventsSeq
	let eventDates = Event.eventDate <$> fetchedEvents allEvents
	let eventDatesLocal = fmap (utcToLocalTime myTz) eventDates
	putStrLn "after the fetching!"
	-- well would be faster to just check the first and last
	-- element... but it's actually shorter to code like this..
	let outOfRangeData = filter (outOfRange date (addDays 1 date)) eventDatesLocal
	if null outOfRangeData
		then return $ (null $ fetchErrors allEvents, JSON.encode allEvents)
		else do
			putStrLn "*** SOME EVENTS ARE NOT IN TIME RANGE"
			print outOfRangeData
			print $ head $ fetchedEvents allEvents
			print $ last $ fetchedEvents allEvents
			return (False, BL.empty)
	where
		outOfRange start end time = time < LocalTime start midnight || time > LocalTime end midnight

getGlobalSettings :: IO GlobalSettings
getGlobalSettings = do
	settingsFolder <- Config.getSettingsFolder
	return GlobalSettings { getSettingsFolder = settingsFolder }

fetchProvider :: T.Text -> GlobalSettings -> Day -> Config.EventSource Value Value -> IO (Either String [Event])
fetchProvider configItemName settings day eventSource = do
	let provider = Config.srcProvider eventSource
	putStrLn $ printf "fetching from %s (provider: %s)"
		(T.unpack $ Config.srcName eventSource) (getModuleName provider)
	evts <- runEitherT $ fmapLT ((T.unpack (Config.srcName eventSource) ++ ": ") ++)
		$ getEvents provider (Config.srcConfig eventSource) settings day (getExtraDataUrl configItemName)
	putStrLn "Done"
	return evts

getExtraDataUrl :: T.Text -> Value -> Url
getExtraDataUrl (TE.encodeUtf8 -> configItemName) key = printf "/getExtraData?configItemName=%s&queryParams=%s" 
	(toUrlParam configItemName) (toUrlParam $ BL.toStrict $ Aeson.encode key)
	where toUrlParam = (fmap $ chr . fromIntegral) . BS.unpack . urlEncode True

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
