{-# LANGUAGE OverloadedStrings #-}

module Ical where

import Data.Time.Clock
import Data.Time.Calendar
import Network.Socket
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.Parsec.Text
import Text.Parsec.Perm
import qualified Text.Parsec as T
import Data.Char (digitToInt)

import qualified Event

icalAddress :: String
icalAddress = "https://www.google.com/calendar/ical/etouzery%40gmail.com/private-d63868fef84ee0826c4ad9bf803048cc/basic.ics"

knownCommands :: [T.Text]
knownCommands = ["BEGIN:VEVENT", "DTSTART:", "DTEND:", "DESCRIPTION:", "SUMMARY:", "END:VEVENT"]

data CalendarRecord = CalendarRecord
			{
				startDate :: UTCTime,
				endDate :: UTCTime,
				description :: String,
				summary :: String
			}
			deriving (Eq, Show)

--eventsTxt = "crap\r\nBEGIN:VEVENT\r\nDTSTART:20121220T113000Z\r\nDTEND:20121220T123000Z\r\nDTSTAMP:20121222T202323Z\r\nUID:libdtse87aoci8tar144sctm7g@google.com\r\nCREATED:20121221T102110Z\r\nDESCRIPTION:test\r\nLAST-MODIFIED:20121221T102116Z\r\nLOCATION:\r\nSEQUENCE:2\r\nSTATUS:CONFIRMED\r\nSUMMARY:sestanek Matej\r\nTRANSP:OPAQUE\r\nEND:VEVENT"

getCalendarEvents :: Day -> Day -> IO [Event.Event]
getCalendarEvents startDay endDay = do
	icalData <- withSocketsDo $ simpleHttp icalAddress
	let icalText = TE.decodeUtf8 $ BL.toStrict icalData
	let parseResult = parseEventsParsec $ filterUnknownEvents icalText
	--let parseResult = parseEventsParsec $ filterUnknownEvents $ T.pack eventsTxt
	case parseResult of
		Left _ -> do putStrLn "parse error"; return []
		Right x -> return $ filterDate startDay endDay x

filterDate :: Day -> Day -> [Event.Event] -> [Event.Event]
filterDate startDay endDay events = filter (eventInDateRange startDay endDay) events

eventInDateRange :: Day -> Day -> Event.Event -> Bool
eventInDateRange startDay endDay event =  eventDay >= startDay && eventDay <= endDay
	where
		eventDay = utctDay $ Event.eventDate event

filterUnknownEvents :: T.Text -> T.Text
filterUnknownEvents input = T.unlines $ filter isKnownCommand (T.lines input)
	where
		isKnownCommand line = any ((flip T.isPrefixOf) line) knownCommands

parseEventsParsec t = parse parseEvents "" t

parseEvents = do
	T.manyTill T.anyChar (T.try $ T.lookAhead parseBegin)
	many parseEvent

eol = many1 $ oneOf "\r\n"

parseEvent = do
	parseBegin
	contents <- permute (CalendarRecord <$$> (T.try startDateParser)
			<||> (T.try endDateParser)
			<||> (T.try descriptionParser)
			<||> (T.try summaryParser))
	parseEnd
	return $ calendarRecordToEvent contents

calendarRecordToEvent :: CalendarRecord -> Event.Event
calendarRecordToEvent record = Event.Event (startDate record)
		Event.Calendar Nothing (T.pack (description record ++ " - " ++ summary record))

parseBegin = do
	string "BEGIN:VEVENT"
	eol

startDateParser = do
	string "DTSTART:"
	parseDateTime

endDateParser = do
	string "DTEND:"
	parseDateTime

parseDateTime = do
	year <- count 4 digit
	month <- count 2 digit
	day <- count 2 digit
	T.char 'T'
	hour <- count 2 digit
	mins <- count 2 digit
	sec <- count 2 digit
	many1 $ noneOf "\r\n"
	eol
	return $ UTCTime
		(fromGregorian (parsedToInteger year) (parsedToInt month) (parsedToInt day))
		(secondsToDiffTime $ (parsedToInteger hour)*3600 + (parsedToInteger mins)*60 + (parsedToInteger sec))

descriptionParser = do
	string "DESCRIPTION:"
	text <- many $ noneOf "\r\n"
	eol
	return text

summaryParser = do
	string "SUMMARY:"
	text <- many $ noneOf "\r\n"
	eol
	return text

parsedToInt :: [Char] -> Int
parsedToInt digits = foldl ((+).(*10)) 0 (map digitToInt digits)

parsedToInteger :: [Char] -> Integer
parsedToInteger = fromIntegral . parsedToInt

parseEnd = do
	string "END:VEVENT"
	optional eol -- at the end of the file there may not be a carriage return.