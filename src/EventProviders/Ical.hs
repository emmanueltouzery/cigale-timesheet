{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell, LambdaCase #-}

module Ical where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.List
import Network.Http.Client
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Parsec.Text
import Text.Parsec hiding ((<|>))
import qualified Data.ByteString.Char8 as B
import System.IO
import qualified System.Directory as Dir
import qualified System.IO.Error as IOEx
import Data.Map as Map hiding (filter, map, foldl)
import Data.Time.Format
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ((<|>))
import Data.Maybe
import Control.Error
import Control.Monad.Trans

import TsEvent
import Util
import EventProvider
import EventProviderSettings

deriveConfigRecord icalConfigDataType
deriveJSON defaultOptions ''IcalConfigRecord

getIcalProvider :: EventProvider IcalConfigRecord ()
getIcalProvider = EventProvider
    {
        getModuleName = "Ical",
        getEvents     = getCalendarEvents,
        getConfigType = members icalConfigDataType,
        getExtraData  = Nothing,
        fetchFieldCts = Nothing
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

getCalendarEvents :: IcalConfigRecord -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getCalendarEvents (IcalConfigRecord icalAddress) settings day _ = do
    timezone <- lift $ getTimeZone (UTCTime day 8)
    let settingsFolder = getSettingsFolder settings
    icalText <- lift $ do
        hasCached <- hasCachedVersionForDay settingsFolder day
        if hasCached
            then readFromCache settingsFolder
            else readFromWWW (TE.encodeUtf8 icalAddress) settingsFolder
    calendarData <- hoistEither $ fmapL show $ parse parseEvents "" icalText
    return $ convertToEvents timezone day calendarData

convertToEvents :: TimeZone -> Day -> [Map String CalendarValue] -> [TsEvent]
convertToEvents tz day keyValues = filterDate tz day $ concatMap (keyValuesToEvents tz) keyValues

readFromWWW :: B.ByteString -> String -> IO Text
readFromWWW icalAddress settingsFolder = do
    putStrLn "reading from WWW"
    icalData <- Util.http GET icalAddress "" concatHandler requestDefaults
    putStrLn "read from WWW"
    let icalText = TE.decodeUtf8 icalData
    putInCache settingsFolder icalText
    return icalText

filterDate :: TimeZone -> Day -> [TsEvent] -> [TsEvent]
filterDate tz day = filter (eventInDateRange tz day)

eventInDateRange :: TimeZone -> Day -> TsEvent -> Bool
eventInDateRange tz day event =  eventDay >= day && eventDay <= day
    where
        eventDay = localDay $ utcToLocalTime tz (TsEvent.eventDate event)

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

makeEvents :: TimeZone -> TsEvent -> LocalTime -> LocalTime -> [TsEvent]
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

keyValuesToEvents :: TimeZone -> Map String CalendarValue -> [TsEvent]
keyValuesToEvents tz records = makeEvents tz baseEvent startDate endDate
    where
        baseEvent = buildBasicEvent descV (localTimeToUTC tz startDate)
        descV = T.concat $ (T.pack . leafValue records) <$> ["DESCRIPTION", "SUMMARY"]
        startDate = fromMaybe (error "No DTSTART!?") $ parseDateNode "DTSTART" records
        endDate = fromMaybe startDate $ parseDateNode "DTEND" records

leafValue :: Map String CalendarValue -> String -> String
leafValue records name = fromMaybe (error $ "No leaf of name " ++ name ++ " " ++ show records) $
    leafText <$> (Map.lookup name records >>= fromLeaf)

buildBasicEvent :: Text -> UTCTime -> TsEvent
buildBasicEvent descV date = TsEvent
   {
       pluginName = getModuleName getIcalProvider,
       eventIcon = "glyphicons-46-calendar",
       eventDate = date,
       desc = descV,
       extraInfo = "",
       fullContents = Nothing
   }

parseBegin :: GenParser st ()
parseBegin = string "BEGIN:VEVENT" >> eol

parseKeyValue :: GenParser st (String, CalendarValue)
parseKeyValue = do
    key <- many $ noneOf ":;"
    propertyParameters <- Map.fromList <$> many parsePropertyParameters
    string ":"
    values <- parseSingleValue `sepBy` string ","
    eol
    return (key, Leaf $ CalendarLeaf propertyParameters values)

isMatch :: GenParser st a -> GenParser st Bool
isMatch parser = isJust <$> optionMaybe (try parser)

singleValueParserMatches :: [(GenParser st String, GenParser st String)]
singleValueParserMatches = [
    (string "\\n", ("\n" ++) <$> parseSingleValue),
    (string "\\", (:) <$> anyChar <*> parseSingleValue),
    (eol >> string " ", parseSingleValue)]

parseSingleValue :: GenParser st String
parseSingleValue = do
    text <- many $ noneOf "\\,\r\n"
    match <- Util.findM (isMatch . fst) singleValueParserMatches
    case match of
        Just matchInfo -> (text ++) <$> snd matchInfo
        Nothing        -> return text

parsePropertyParameters :: GenParser st (String, String)
parsePropertyParameters = do
    string ";"
    key   <- many $ noneOf "="
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

parseDateNode :: String -> Map String CalendarValue -> Maybe LocalTime
parseDateNode key records = do
    dateInfo <- Map.lookup key records >>= fromLeaf
    case Map.lookup "VALUE" $ propParams dateInfo of
        Just "DATE" -> parseDateOnlyNode key $ leafText dateInfo
        _ -> parseMaybe parseDateTime $ T.pack $ leafText dateInfo

parseDateOnlyNode :: String -> String -> Maybe LocalTime
parseDateOnlyNode key (T.pack -> nodeText) = dayTime <$> parseMaybe parseDate nodeText
    where dayTime time = case key of
              "DTSTART" -> time
              "DTEND"   -> time { localTimeOfDay = TimeOfDay 23 59 59 } -- end of the day
              _         -> error $ "parseDateOnlyNode: " ++ show key

parseDate :: GenParser st LocalTime
parseDate = do
    date <- fromGregorian <$> parseNum 4 <*> parseNum 2 <*> parseNum 2
    return $ LocalTime date (TimeOfDay 0 0 0)

parseDateTime :: GenParser st LocalTime
parseDateTime = do
    date  <- parseDate
    char 'T'
    timeOfDay <- TimeOfDay <$> parseNum 2 <*> parseNum 2 <*> parseNum 2
    return $ date { localTimeOfDay = timeOfDay }

-- at the end of the file there may not be a carriage return.
parseEnd :: GenParser st ()
parseEnd = string "END:VEVENT" >> optional eol

hasCachedVersionForDay :: String -> Day -> IO Bool
hasCachedVersionForDay settingsFolder day =
    cachedVersionDate settingsFolder >>= \case
        Nothing         -> return False
        Just cachedDate -> return $ day < cachedDate

cacheFilename :: String -> String
cacheFilename settingsFolder = settingsFolder ++ "cached-calendar.ical"

cachedVersionDate :: String -> IO (Maybe Day)
cachedVersionDate settingsFolder = do
    modifTime <- IOEx.tryIOError $ Dir.getModificationTime
        $ cacheFilename settingsFolder
    return $ utctDay <$> hush modifTime

readFromCache :: String -> IO Text
readFromCache settingsFolder = do
    putStrLn "reading calendar from cache!"
    T.pack <$> readFile (cacheFilename settingsFolder)

putInCache :: String -> Text -> IO ()
putInCache settingsFolder text =
    withFile (cacheFilename settingsFolder) WriteMode (`T.hPutStr` text)
