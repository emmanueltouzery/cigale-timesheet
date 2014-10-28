{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell, LambdaCase #-}

module Ical where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.List
import Network.Socket
import Network.Http.Client
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec.Text
import Text.Parsec hiding ((<|>))
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
import Control.Monad.Trans

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

getIcalProvider :: EventProvider IcalRecord ()
getIcalProvider = EventProvider
	{
		getModuleName = "Ical",
		getEvents = getCalendarEvents,
		getConfigType = members $(thGetTypeDesc ''IcalRecord),
		getExtraData = Nothing
	}

-- we can have components or objects, which are between BEGIN and END blocks,
-- and can be recursive:
--
--     BEGIN:VTIMEZONE
--     TZID:US-Eastern
--     BEGIN:STANDARD
--     DTSTART:19971026T020000
--     TZOFFSETFROM:-0400
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19971026T02000
--     TZOFFSETFROM:-0500
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE0
--
-- I map those with a tree structure.
--
-- And also: http://tools.ietf.org/html/rfc2445#section-4.1.1
--
--    Some properties allow a list of parameters. Each property parameter
--    in a list of property parameters MUST be separated by a SEMICOLON
--    character (US-ASCII decimal 59).
-- 
--    Property parameters with values containing a COLON, a SEMICOLON or a
--    COMMA character MUST be placed in quoted text.
-- 
--    For example, in the following properties a SEMICOLON is used to
--    separate property parameters from each other, and a COMMA is used to
--    separate property values in a value list.
-- 
--      ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:MAILTO:
--       jsmith@host.com
-- 
--      RDATE;VALUE=DATE:19970304,19970504,19970704,19970904
--
-- Due to that, I have propParams (property parameters) and a list for the property values.
data CalendarLeaf = CalendarLeaf { propParams :: Map String String, propValues :: [String] } deriving (Show, Eq)
data CalendarValue = Leaf CalendarLeaf
	| SubLevel (Map String CalendarValue) deriving (Show, Eq)

leafText :: CalendarLeaf -> String
leafText = intercalate ", " . propValues

fromLeaf :: CalendarValue -> Maybe CalendarLeaf
fromLeaf (Leaf x) = Just x
fromLeaf _ = Nothing

getCalendarEvents :: IcalRecord -> GlobalSettings -> Day -> (() -> Url) -> EitherT String IO [Event.Event]
getCalendarEvents (IcalRecord icalAddress) settings day _ = do
	timezone <- lift $ getTimeZone (UTCTime day 8)
	let settingsFolder = getSettingsFolder settings
	icalText <- lift $ do
		hasCached <- hasCachedVersionForDay settingsFolder day
		if hasCached
			then readFromCache settingsFolder
			else readFromWWW (B.pack icalAddress) settingsFolder
	calendarData <- hoistEither $ fmapL show $ parse parseEvents "" icalText
	return $ convertToEvents timezone day calendarData

convertToEvents :: TimeZone -> Day -> [Map String CalendarValue] -> [Event.Event]
convertToEvents tz day keyValues = filterDate tz day $ concatMap (keyValuesToEvents tz) keyValues

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

parseEvents :: GenParser st [Map String CalendarValue]
parseEvents = do
	manyTill anyChar (try $ lookAhead parseBegin)
	many parseEvent

parseEvent :: GenParser st (Map String CalendarValue)
parseEvent = do
	parseBegin
	keyValues <- manyTill
		(try parseSubLevel <|> try parseKeyValue)
		(try parseEnd)
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
		leafValue name = fromMaybe (error $ "No leaf of name " ++ name ++ " " ++ show records) $
			leafText <$> (Map.lookup name records >>= fromLeaf)
		descV = T.concat [T.pack $ leafValue "DESCRIPTION",
				 T.pack $ leafValue "SUMMARY"]
		startDate = parseDateNode "DTSTART" records
		endDate = parseDateNode "DTEND" records

parseBegin :: GenParser st String
parseBegin = string "BEGIN:VEVENT" >> eol

parseKeyValue :: GenParser st (String, CalendarValue)
parseKeyValue = do
	key <- many $ noneOf ":;"
	propertyParameters <- Map.fromList <$> many parsePropertyParameters
	string ":"
	values <- parseSingleValue `sepBy` string ","
	eol
	return (key, Leaf $ CalendarLeaf propertyParameters values)

parseSingleValue :: GenParser st String
parseSingleValue = do
	text <- many1 $ noneOf "\\,\r\n"
	isBackslash <- isJust <$> optionMaybe (string "\\")
	if isBackslash
		then do
			chr <- anyChar
			((text ++ [chr]) ++) <$> parseSingleValue
		else return text

parsePropertyParameters :: GenParser st (String, String)
parsePropertyParameters = do
	string ";"
	key <- many $ noneOf "="
	string "="
	value <- many $ noneOf ";:"
	return (key, value)

parseSubLevel :: GenParser st (String, CalendarValue)
parseSubLevel = do
	string "BEGIN:"
	value <- many $ noneOf "\r\n"
	eol
	subcontents <- manyTill parseKeyValue (try (do string $ "END:" ++ value; eol))
	return (value, SubLevel $ Map.fromList subcontents)

parseDateNode :: String -> Map String CalendarValue -> LocalTime
parseDateNode key records = fromMaybe (error $ "Didn't find a parseable leaf for the date " ++ key) $ do
	dateInfo <- Map.lookup key records >>= fromLeaf
	case Map.lookup "VALUE" $ propParams dateInfo of
		Just "DATE" -> parseDateOnlyNode key $ leafText dateInfo
		_ -> parseMaybe parseDateTime $ T.pack $ leafText dateInfo

parseDateOnlyNode :: String -> String -> Maybe LocalTime
parseDateOnlyNode key (T.pack -> nodeText) = dayTime <$> parseMaybe parseDate nodeText
	where dayTime time = case key of
		"DTSTART" -> time
		"DTEND" -> time { localTimeOfDay = TimeOfDay 23 59 59 } -- end of the day

parseDate :: GenParser st LocalTime
parseDate = do
	year <- parseNum 4
	month <- parseNum 2
	day <- parseNum 2
	return $ LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)

parseDateTime :: GenParser st LocalTime
parseDateTime = do
	date <- parseDate
	hours <- char 'T' >> parseNum 2
	mins <- parseNum 2
	sec <- parseNum 2
	return $ date { localTimeOfDay = TimeOfDay hours mins sec }

-- at the end of the file there may not be a carriage return.
parseEnd :: GenParser st ()
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
	T.pack <$> readFile (cacheFilename settingsFolder)

putInCache :: String -> T.Text -> IO ()
putInCache settingsFolder text = withFile (cacheFilename settingsFolder) WriteMode (`T.hPutStr` text)

eol :: GenParser st String
eol = many1 $ oneOf "\r\n"
