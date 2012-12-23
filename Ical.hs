{-# LANGUAGE OverloadedStrings #-}

module Ical where

import Data.Time.Clock
import Network.Socket
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.Parsec.Text
import qualified Text.Parsec as T
import Data.Char (digitToInt)

import qualified Event

icalAddress :: String
icalAddress = "https://www.google.com/calendar/ical/etouzery%40gmail.com/private-d63868fef84ee0826c4ad9bf803048cc/basic.ics"

data CalendarInfo = StartDate [Int] --UTCTime
	| EndDate UTCTime
	| Description String
	deriving (Eq, Show)

eventsTxt = "crap\r\nBEGIN:VEVENT\r\nDTSTART:20121220T113000Z\r\nDTEND:20121220T123000Z\r\nDTSTAMP:20121222T202323Z\r\nUID:libdtse87aoci8tar144sctm7g@google.com\r\nCREATED:20121221T102110Z\r\nDESCRIPTION:\r\nLAST-MODIFIED:20121221T102116Z\r\nLOCATION:\r\nSEQUENCE:2\r\nSTATUS:CONFIRMED\r\nSUMMARY:sestanek Matej\r\nTRANSP:OPAQUE\r\nEND:VEVENT"

getCalendarEvents :: IO [Event.Event]
getCalendarEvents = do
	--icalData <- withSocketsDo $ simpleHttp icalAddress
	--let icalText = TE.decodeUtf8 $ BL.toStrict icalData
	--let parseResult = parseEventsParsec icalText
	--let parseResult = parseEventsParsec eventsTxt
	let parseResult = parseEventsParsec $ T.pack eventsTxt
	case parseResult of
		Left _ -> putStrLn "parse error"
		Right x -> print x
	-- print $ parseEvents icalText
	return []


parseEventsParsec t = parse parseEvents "" t

parseEvents = do
	T.manyTill T.anyChar (T.try $ T.lookAhead parseBegin)
	many parseEvent

--eol = string "\r\n" --many1 $ oneOf "\r\n"
eol = many1 $ oneOf "\r\n"

parseEvent = do
	parseBegin
	contents <- many1 parseRow
	parseEnd
	return contents

parseBegin = do
	string "BEGIN:VEVENT"
	eol

parseRow = do
	notFollowedBy $ string "END:VEVENT"
	notFollowedBy $ string "BEGIN:VEVENT"
	contents <-  (T.try startDate) <|> unknownCalendarInfo
	--contents <- unknownCalendarInfo
	eol
	return contents

unknownCalendarInfo = do
	dataS <- many1 $ noneOf "\r\n"
	--return $ Just $ Description dataS
	return Nothing

startDate = do
	string "DTSTART:"
	year <- count 4 digit
	month <- count 2 digit
	day <- count 2 digit
	T.char 'T'
	hour <- count 2 digit
	mins <- count 2 digit
	sec <- count 2 digit
	many1 $ noneOf "\r\n"
	return $ Just $ StartDate $ map parsedToInt [year,month,day,hour,mins,sec]

parsedToInt :: [Char] -> Int
parsedToInt digits = foldl ((+).(*10)) 0 (map digitToInt digits)

parseEnd = do
	string "END:VEVENT"
	optional eol -- at the end of the file there may not be a carriage return.


-- icalFile = do
-- 	h <- header
-- 	entries <- many entry
-- 	f <- footer
-- 	return entries
-- 
-- header = endBy $ string "X-WR-TIMEZONE:Europe/Belgrade"
-- 
-- entry = endBy $ string "END:VEVENT"
-- 
-- footer = string "END:VCALENDAR"
-- 
-- --parseEvents :: T.Text -> [Event.Event]
-- parseEvents = parse icalFile "" 
-- 
-- -- events :: GenParser T.Text st [T.Text]
-- -- events = do
-- -- 	skipMany $ noneOf "BEGIN:VEVENT"
-- -- 	string "DTSTART:"
-- -- 	digit
-- -- 
-- -- --tillNextEventStart :: GenParser T.Text st [T.Text]
-- -- tillNextEventStart = many (noneOf "BEGIN:VEVENT")
