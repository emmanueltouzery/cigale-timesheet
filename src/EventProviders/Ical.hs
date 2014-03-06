{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell #-}

module Ical where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Network.Socket
import Network.Http.Client
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.Parsec
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

import Text.Regex.PCRE.Rex

import Event
--import qualified Settings
import qualified Util
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

--eventsTxt = "crap\r\nBEGIN:VEVENT\r\nDTSTART:20121220T113000Z\r\nDTEND:20121220T123000Z\r\nDTSTAMP:20121222T202323Z\r\nUID:libdtse87aoci8tar144sctm7g@google.com\r\nCREATED:20121221T102110Z\r\nDESCRIPTION:test\r\nLAST-MODIFIED:20121221T102116Z\r\nLOCATION:\r\nSEQUENCE:2\r\nSTATUS:CONFIRMED\r\nSUMMARY:sestanek Matej\r\nTRANSP:OPAQUE\r\nEND:VEVENT"

data CalendarValue = Leaf String | SubLevel (Map String CalendarValue) deriving (Show, Eq)

fromLeaf :: CalendarValue -> String
fromLeaf (Leaf a) = a
fromLeaf _ = "Error: expected a leaf!"

getCalendarEvents :: IcalRecord -> GlobalSettings -> Day -> IO [Event.Event]
getCalendarEvents (IcalRecord icalAddress) settings day = do
	timezone <- getCurrentTimeZone
	hasCached <- hasCachedVersionForDay settingsFolder day
	icalText <- if hasCached
		then readFromCache settingsFolder
		else readFromWWW (B.pack icalAddress) settingsFolder
	let parseResult = parseEventsParsec icalText
	--print parseResult
	case parseResult of
		Left pe -> do
			putStrLn $ "iCal: parse error: " ++ Util.displayErrors pe
			putStrLn $ "line:col: " 
				++ show (sourceLine $ errorPos pe) 
				++ ":" ++ show (sourceColumn $ errorPos pe)
			error "Ical parse error, aborting"
			--return []
		Right x -> return $ convertToEvents timezone day x
	where
		settingsFolder = getSettingsFolder settings

convertToEvents :: TimeZone -> Day -> [Map String CalendarValue] -> [Event.Event]
convertToEvents tz day keyValues = filterDate day events
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

filterDate :: Day -> [Event.Event] -> [Event.Event]
filterDate day = filter (eventInDateRange day)

eventInDateRange :: Day -> Event.Event -> Bool
eventInDateRange day event =  eventDay >= day && eventDay <= day
	where
		eventDay = utctDay $ Event.eventDate event

parseEventsParsec :: T.Text -> Either ParseError [Map String CalendarValue]
parseEventsParsec = parse parseEvents ""

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
makeEvents tz base start end | localDay end == localDay start = [base
	{
		eventDate = startUtc,
		extraInfo = T.concat["End: ", utctDayTimeStr endUtc,
			"; duration: ", Util.formatDurationSec $ diffUTCTime endUtc startUtc]
	}]
	where
		startUtc = localTimeToUTC tz start
		endUtc = localTimeToUTC tz end
makeEvents tz base start end = makeEvents tz base start (start {localTimeOfDay = TimeOfDay 23 59 0}) ++
		makeEvents tz base (LocalTime (addDays 1 (localDay start)) (TimeOfDay 0 0 0)) end

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

utctDayTimeStr :: UTCTime -> T.Text
utctDayTimeStr time = T.pack $ formatTime defaultTimeLocale "%R" time

parseBegin :: T.GenParser st String
parseBegin = do
	string "BEGIN:VEVENT"
	eol

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
parseDateNode key records = case Map.lookup key records of
	Just value -> parseDateTime $ fromLeaf value
	Nothing -> case Map.lookup (key ++ ";VALUE=DATE") records of
		Just value -> dayTime $ parseDate $ fromLeaf value
		Nothing -> error $ "Didn't find a leaf for the date " ++ key
	where
		dayTime time = case key of
			"DTSTART" -> time
			"DTEND" -> time { localTimeOfDay = TimeOfDay 23 59 59 } -- end of the day

parseDate :: String -> LocalTime
parseDate [rex|(?{read -> year}\d{4})(?{read -> month}\d\d)(?{read -> day}\d\d)|] =
	LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)
parseDate date@_ = error $ "unrecognized iCal date: " ++ date

parseDateTime :: String -> LocalTime
parseDateTime [rex|(?{read -> year}\d{4})(?{read -> month}\d\d)(?{read -> day}\d\d)T
		(?{read -> hour}\d\d)(?{read -> mins}\d\d)(?{read -> sec}\d\d)|] =
	LocalTime (fromGregorian year month day) (TimeOfDay hour mins sec)
parseDateTime date@_ = error $ "unrecognized iCal datetime: " ++ date

parseEnd :: T.GenParser st ()
parseEnd = do
	string "END:VEVENT"
	optional eol -- at the end of the file there may not be a carriage return.

hasCachedVersionForDay :: String -> Day -> IO Bool
hasCachedVersionForDay settingsFolder day = do
	cachedDateMaybe <- cachedVersionDate settingsFolder
	case cachedDateMaybe of
		Nothing -> return False
		Just cachedDate -> return $ day < cachedDate

cacheFilename :: String -> String
cacheFilename settingsFolder = settingsFolder ++ "cached-calendar.ical"

cachedVersionDate :: String -> IO (Maybe Day)
cachedVersionDate settingsFolder = do
	let fname = cacheFilename settingsFolder
	modifTime <- IOEx.tryIOError $ Dir.getModificationTime fname
	case modifTime of
		Left _ -> return Nothing
		Right modif -> return $ Just $ utctDay modif

readFromCache :: String -> IO T.Text
readFromCache settingsFolder = do
	putStrLn "reading calendar from cache!"
	let fname = cacheFilename settingsFolder
	fmap T.pack (readFile fname)


putInCache :: String -> T.Text -> IO ()
putInCache settingsFolder text = do
	let fname = cacheFilename settingsFolder
	fileH <- openFile fname WriteMode
	T.hPutStr fileH text
	hClose fileH

eol :: T.GenParser st String
eol = many1 $ oneOf "\r\n"
