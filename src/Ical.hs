{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Ical where

import Data.Time.Clock
import Data.Time.Calendar
import Network.Socket
import Network.HTTP.Conduit
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import System.IO
import qualified System.Directory as Dir
import qualified System.IO.Error as IOEx
import Data.Map as Map hiding (filter, map)
import Data.Time.Clock.POSIX
import System.Time.Utils
import Data.Time.Format
import System.Locale

import Text.Regex.PCRE.Rex

import qualified Event
import qualified Settings
import qualified Util

icalAddress :: String
icalAddress = "https://www.google.com/calendar/ical/etouzery%40gmail.com/private-d63868fef84ee0826c4ad9bf803048cc/basic.ics"

--eventsTxt = "crap\r\nBEGIN:VEVENT\r\nDTSTART:20121220T113000Z\r\nDTEND:20121220T123000Z\r\nDTSTAMP:20121222T202323Z\r\nUID:libdtse87aoci8tar144sctm7g@google.com\r\nCREATED:20121221T102110Z\r\nDESCRIPTION:test\r\nLAST-MODIFIED:20121221T102116Z\r\nLOCATION:\r\nSEQUENCE:2\r\nSTATUS:CONFIRMED\r\nSUMMARY:sestanek Matej\r\nTRANSP:OPAQUE\r\nEND:VEVENT"

data CalendarValue = Leaf String | SubLevel (Map String CalendarValue) deriving (Show, Eq)

fromLeaf :: CalendarValue -> String
fromLeaf (Leaf a) = a
fromLeaf _ = "Error: expected a leaf!"

getCalendarEvents :: Day -> Day -> IO [Event.Event]
getCalendarEvents startDay endDay = do
	hasCached <- hasCachedVersionForDay endDay
	icalText <- if hasCached
		then readFromCache
		else readFromWWW
	let parseResult = parseEventsParsec icalText
	--print parseResult
	case parseResult of
		Left pe -> do
			putStrLn $ "iCal: parse error: " ++ Util.displayErrors pe
			putStrLn $ "line:col: " 
				++ (show $ sourceLine $ errorPos pe) 
				++ ":" ++ (show $ sourceColumn $ errorPos pe)
			return []
		Right x -> return $ convertToEvents startDay endDay x

convertToEvents :: Day -> Day -> [Map String CalendarValue] -> [Event.Event]
convertToEvents startDay endDay keyValues = filterDate startDay endDay events
	where
		events = map keyValuesToEvent keyValues

readFromWWW :: IO T.Text
readFromWWW = do
	icalData <- withSocketsDo $ simpleHttp icalAddress
	let icalText = TE.decodeUtf8 $ Util.toStrict1 icalData
	putInCache icalText
	return icalText

filterDate :: Day -> Day -> [Event.Event] -> [Event.Event]
filterDate startDay endDay events = filter (eventInDateRange startDay endDay) events

eventInDateRange :: Day -> Day -> Event.Event -> Bool
eventInDateRange startDay endDay event =  eventDay >= startDay && eventDay <= endDay
	where
		eventDay = utctDay $ Event.eventDate event

parseEventsParsec :: T.Text -> Either ParseError [Map String CalendarValue]
parseEventsParsec t = parse parseEvents "" t

parseEvents :: T.GenParser st [Map String CalendarValue]
parseEvents = do
	T.manyTill T.anyChar (T.try $ T.lookAhead parseBegin)
	many parseEvent

parseEvent :: T.GenParser st (Map String CalendarValue)
parseEvent = do
	parseBegin
	keyValues <- manyTill
		((T.try parseSubLevel) <|> (T.try parseKeyValue))
		(T.try parseEnd)
	return $ Map.fromList keyValues

keyValuesToEvent :: Map String CalendarValue -> Event.Event
keyValuesToEvent records = Event.Event startDate Event.Calendar Nothing desc extraInfo
	where
		desc = T.concat [T.pack $ fromLeaf $ records ! "DESCRIPTION",
				 T.pack $ fromLeaf $ records ! "SUMMARY"]
		startDate = parseDate $ fromLeaf $ records ! "DTSTART"
		endDate = parseDate $ fromLeaf $ records ! "DTEND"
		extraInfo = T.concat["End: ", utctDayTimeStr endDate,
			" Duration: ", formatDurationSec $ diffUTCTime endDate startDate]

formatDurationSec :: NominalDiffTime -> T.Text
formatDurationSec seconds = T.concat [T.pack hours, ":", T.pack minutes]
	where
		secondsI = round seconds :: Int
		hours = show $ secondsI `div` 3600
		minutes = show $ (secondsI `mod` 3600) `div` 60

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

parseDate :: String -> UTCTime
parseDate [rex|(?{read -> year}\d{4})(?{read -> month}\d\d)(?{read -> day}\d\d)T
		(?{read -> hour}\d\d)(?{read -> mins}\d\d)(?{read -> sec}\d\d)|] =
	UTCTime (fromGregorian year month day) (secondsToDiffTime secOfDay)
	where
		secOfDay = hour*3600 + mins*60 + sec
parseDate date@_ = error $ "unrecognized iCal date: " ++ date

parseEnd :: T.GenParser st ()
parseEnd = do
	string "END:VEVENT"
	optional eol -- at the end of the file there may not be a carriage return.

hasCachedVersionForDay :: Day -> IO Bool
hasCachedVersionForDay day = do
	cachedDateMaybe <- cachedVersionDate
	case cachedDateMaybe of
		Nothing -> return False
		Just cachedDate -> return $ day < cachedDate

cacheFilename :: IO String
cacheFilename = do
	settingsFolder <- Settings.getSettingsFolder
	return $ settingsFolder ++ "cached-calendar.ical"

cachedVersionDate :: IO (Maybe Day)
cachedVersionDate = do
	fname <- cacheFilename
	modifTime <- IOEx.tryIOError $ Dir.getModificationTime fname
	case modifTime of
		Left _ -> return Nothing
		Right modif -> return $ Just $ utctDay $ posixSecondsToUTCTime $ clockTimeToEpoch modif
		--Right modif -> return $ Just $ utctDay modif

readFromCache :: IO T.Text
readFromCache = do
	putStrLn "reading calendar from cache!"
	fname <- cacheFilename
	fmap T.pack (readFile fname)


putInCache :: T.Text -> IO ()
putInCache text = do
	fname <- cacheFilename
	fileH <- openFile fname WriteMode
	T.hPutStr fileH text
	hClose fileH

eol :: T.GenParser st String
eol = many1 $ oneOf "\r\n"
