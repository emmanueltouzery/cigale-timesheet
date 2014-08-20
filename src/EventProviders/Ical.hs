{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell, LambdaCase #-}

module Ical where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Network.Socket
import Network.Http.Client
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.ByteString.Char8 as B
import System.IO
import qualified System.Directory as Dir
import qualified System.IO.Error as IOEx
import Data.Map as Map hiding (filter, map, foldl)
import Data.Time.Clock.POSIX
import System.Time.Utils
import Data.Time.Format
import System.Locale
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ((<$>), (<|>))
import Data.Maybe
import Control.Error

import Text.Regex.PCRE.Rex

import Event
--import qualified Settings
import qualified Util
import Util (parseMaybe, parseNum)
import EventProvider


data IcalRecord = IcalRecord
	{
		icalUrl :: String
	} deriving Show
deriveJSON defaultOptions ''IcalRecord

getIcalProvider :: EventProvider IcalRecord
getIcalProvider = EventProvider
	{
		getModuleName = "Ical",
		getEvents = getCalendarEvents,
		getConfigType = members $(thGetTypeDesc ''IcalRecord)
	}

data CalendarValue = Leaf String | SubLevel (Map String CalendarValue) deriving (Show, Eq)

fromLeaf :: CalendarValue -> String
fromLeaf (Leaf a) = a
fromLeaf _ = "Error: expected a leaf!"

getCalendarEvents :: IcalRecord -> GlobalSettings -> Day -> IO [Event.Event]
getCalendarEvents (IcalRecord icalAddress) settings day = do
	timezone <- getTimeZone (UTCTime day 8)
	hasCached <- hasCachedVersionForDay settingsFolder day
	icalText <- if hasCached
		then readFromCache settingsFolder
		else readFromWWW (B.pack icalAddress) settingsFolder
	return $ convertToEvents timezone day
		$ Util.parsecError parseEvents "Ical.getCalendarEvents" icalText
	where
		settingsFolder = getSettingsFolder settings

convertToEvents :: TimeZone -> Day -> [Map String CalendarValue] -> [Event.Event]
convertToEvents tz day keyValues = filterDate tz day events
	where
		events = concatMap (keyValuesToEvents tz) keyValues

readFromWWW :: B.ByteString -> String -> IO T.Text
readFromWWW icalAddress settingsFolder = do
	putStrLn "reading from WWW"
	--icalData <- withSocketsDo $ Util.http icalAddress "" concatHandler $ do
	icalData <- Util.http icalAddress "" concatHandler $ http GET icalAddress
	putStrLn "read from WWW"
	let icalText = TE.decodeUtf8 icalData
	putInCache settingsFolder icalText
	return icalText

filterDate :: TimeZone -> Day -> [Event.Event] -> [Event.Event]
filterDate tz day = filter (eventInDateRange tz day)

eventInDateRange :: TimeZone -> Day -> Event.Event -> Bool
eventInDateRange tz day event =  eventDay >= day && eventDay <= day
	where
		eventDay = localDay $ utcToLocalTime tz (Event.eventDate event)

parseEvents :: T.GenParser st [Map String CalendarValue]
parseEvents = do
	T.manyTill T.anyChar (T.try $ T.lookAhead parseBegin)
	many parseEvent

parseEvent :: T.GenParser st (Map String CalendarValue)
parseEvent = do
	parseBegin
	keyValues <- manyTill
		(T.try parseSubLevel <|> T.try parseKeyValue)
		(T.try parseEnd)
	return $ Map.fromList keyValues

makeEvents :: TimeZone -> Event.Event -> LocalTime -> LocalTime -> [Event.Event]
makeEvents tz base start end
	| localDay end == localDay start =
		[base
		{
			eventDate = startUtc,
			extraInfo = T.concat["End: ", T.pack $ formatTime defaultTimeLocale "%R" end,
				"; duration: ", Util.formatDurationSec $ diffUTCTime endUtc startUtc]
		}]
	| otherwise = makeEvents tz base start (start {localTimeOfDay = TimeOfDay 23 59 0}) ++
		makeEvents tz base (LocalTime (addDays 1 (localDay start)) (TimeOfDay 0 0 0)) end
	where
		startUtc = localTimeToUTC tz start
		endUtc = localTimeToUTC tz end

keyValuesToEvents :: TimeZone -> Map String CalendarValue -> [Event.Event]
keyValuesToEvents tz records = makeEvents tz baseEvent startDate endDate
	where
		baseEvent = Event.Event
			{
				pluginName = getModuleName getIcalProvider,
				eventIcon = "glyphicon-calendar",
				eventDate = localTimeToUTC tz startDate,
				desc = descV,
				extraInfo = "",
				fullContents = Nothing
			}
		leafValue name = case Map.lookup name records of
			Just value -> fromLeaf value
			Nothing -> error $ "No leaf of name " ++ name ++ " " ++ show records
		descV = T.concat [T.pack $ leafValue "DESCRIPTION",
				 T.pack $ leafValue "SUMMARY"]
		startDate = parseDateNode "DTSTART" records
		endDate = parseDateNode "DTEND" records

parseBegin :: T.GenParser st String
parseBegin = string "BEGIN:VEVENT" >> eol

parseKeyValue :: T.GenParser st (String, CalendarValue)
parseKeyValue = do
	key <- many $ noneOf ":"
	string ":"
	value <- many $ noneOf "\r\n"
	eol
	return (key, Leaf value)

parseSubLevel :: T.GenParser st (String, CalendarValue)
parseSubLevel = do
	string "BEGIN:"
	value <- many $ noneOf "\r\n"
	eol
	subcontents <- manyTill parseKeyValue (T.try (do string $ "END:" ++ value; eol))
	return (value, SubLevel $ Map.fromList subcontents)

parseDateNode :: String -> Map String CalendarValue -> LocalTime
parseDateNode key records = fromMaybe (error $ "Didn't find a parseable leaf for the date " ++ key)
	$ parseDateTimeNode key records <|> parseDateOnlyNode key records

parseDateTimeNode :: String -> Map String CalendarValue -> Maybe LocalTime
parseDateTimeNode key records = fromLeaf <$> Map.lookup key records
	>>= parseMaybe parseDateTime . T.pack

parseDateOnlyNode :: String -> Map String CalendarValue -> Maybe LocalTime
parseDateOnlyNode key records = do
	nodeText <- T.pack . fromLeaf <$> Map.lookup (key ++ ";VALUE=DATE") records
	dayTime <$> parseMaybe parseDate nodeText
	where
		dayTime time = case key of
			"DTSTART" -> time
			"DTEND" -> time { localTimeOfDay = TimeOfDay 23 59 59 } -- end of the day

parseDate :: T.GenParser st LocalTime
parseDate = do
	year <- parseNum 4
	month <- parseNum 2
	day <- parseNum 2
	return $ LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	date <- parseDate
	hours <- char 'T' >> parseNum 2
	mins <- parseNum 2
	sec <- parseNum 2
	return $ date { localTimeOfDay = TimeOfDay hours mins sec }

-- at the end of the file there may not be a carriage return.
parseEnd :: T.GenParser st ()
parseEnd = string "END:VEVENT" >> optional eol

hasCachedVersionForDay :: String -> Day -> IO Bool
hasCachedVersionForDay settingsFolder day =
	cachedVersionDate settingsFolder >>= \case
		Nothing -> return False
		Just cachedDate -> return $ day < cachedDate

cacheFilename :: String -> String
cacheFilename settingsFolder = settingsFolder ++ "cached-calendar.ical"

cachedVersionDate :: String -> IO (Maybe Day)
cachedVersionDate settingsFolder = do
	modifTime <- IOEx.tryIOError $ Dir.getModificationTime
		$ cacheFilename settingsFolder
	return $ utctDay <$> hush modifTime

readFromCache :: String -> IO T.Text
readFromCache settingsFolder = do
	putStrLn "reading calendar from cache!"
	T.pack <$> (readFile $ cacheFilename settingsFolder)

putInCache :: String -> T.Text -> IO ()
putInCache settingsFolder text = withFile (cacheFilename settingsFolder) WriteMode
	(\h -> T.hPutStr h text)

eol :: T.GenParser st String
eol = many1 $ oneOf "\r\n"
