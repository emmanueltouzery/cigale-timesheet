{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts #-}

module IcalSpec where

import Test.Hspec
import Test.HUnit

import Data.Text (Text)
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import qualified Text.Parsec as T

import Str
import Ical
import TsEvent
import EventProvider

import TestUtil

spec :: Spec
spec = do
    let tz = utc
    testBasicEvent tz
    testThroughMidnightEvent tz
    testWholeDayEvent tz
    testNoEndEvent tz
    testSlashes tz

testBasicEvent :: TimeZone -> Spec
testBasicEvent tz = it "parses basic ICAL event" $ do
    let source = [strT|
        BEGIN:VEVENT
        DTSTART:20130417T073000Z
        DTEND:20130417T090000Z
        DTSTAMP:20130419T192234Z
        UID:d7anctkba3qoui0qcru9samr0o@google.com
        CREATED:20130417T131454Z
        DESCRIPTION:
        LAST-MODIFIED:20130417T131454Z
        LOCATION:
        SEQUENCE:0
        STATUS:CONFIRMED
        SUMMARY:spent a lot of time researching bus tables\, for position records
        TRANSP:OPAQUE
        END:VEVENT
            |]
    let expected = TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 17)
                (secondsToDiffTime $ 7*3600+30*60),
            desc = "spent a lot of time researching bus tables, for position records",
            extraInfo = "End: 09:00; duration: 1:30",
            fullContents = Nothing
        }
    testParsecExpectTransform (head . keyValuesToEvents tz . head) source parseEvents expected

testThroughMidnightEvent :: TimeZone -> Spec
testThroughMidnightEvent tz = it "parses basic ICAL event through midnight" $ do
    let source = [strT|
        BEGIN:VEVENT
        DTSTART:20130417T233000Z
        DTEND:20130418T003000Z
        RRULE:FREQ=WEEKLY;UNTIL=20141020;BYDAY=SU,MO
        DTSTAMP:20130419T192234Z
        UID:d7anctkba3qoui0qcru9samr0o@google.com
        CREATED:20130417T131454Z
        DESCRIPTION:
        LAST-MODIFIED:20130417T131454Z
        LOCATION:
        SEQUENCE:0
        STATUS:CONFIRMED
        SUMMARY:spent a lot of time researching bus tables for position records
        TRANSP:OPAQUE
        END:VEVENT
            |]
    let expected = [TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 17)
                (secondsToDiffTime $ 23*3600+30*60),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 0:29",
            fullContents = Nothing
        }, TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 18)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 00:30; duration: 0:30",
            fullContents = Nothing
        }]
    testIcalParse tz source parseEvents expected

testWholeDayEvent :: TimeZone -> Spec
testWholeDayEvent tz = it "parses whole day ICAL event" $ do
    let source = [strT|
        BEGIN:VEVENT
        DTSTART;VALUE=DATE:20140331
        DTEND;VALUE=DATE:20140402
        DTSTAMP:20130419T192234Z
        UID:d7anctkba3qoui0qcru9samr0o@google.com
        CREATED:20130417T131454Z
        DESCRIPTION:
        LAST-MODIFIED:20130417T131454Z
        LOCATION:
        SEQUENCE:0
        STATUS:CONFIRMED
        SUMMARY:spent a lot of time researching bus tables for position records
        TRANSP:OPAQUE
        END:VEVENT
            |]
    let expected = [TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2014 3 31)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }, TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2014 4 1)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }, TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2014 4 2)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }]
    testIcalParse tz source parseEvents expected

testNoEndEvent :: TimeZone -> Spec
testNoEndEvent tz = it "parses event no end date" $ do
    let source = [strT|
        BEGIN:VEVENT
        DTSTART:20130417T073000Z
        DTSTAMP:20130419T192234Z
        UID:d7anctkba3qoui0qcru9samr0o@google.com
        CREATED:20130417T131454Z
        DESCRIPTION:
        LAST-MODIFIED:20130417T131454Z
        LOCATION:
        SEQUENCE:0
        STATUS:CONFIRMED
        SUMMARY:spent a lot of time researching bus tables\, for position records
        TRANSP:OPAQUE
        END:VEVENT
            |]
    let expected = TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 17)
                (secondsToDiffTime $ 7*3600+30*60),
            desc = "spent a lot of time researching bus tables, for position records",
            extraInfo = "End: 07:30; duration: 0:00",
            fullContents = Nothing
        }
    testIcalParse tz source parseEvents [expected]

testSlashes :: TimeZone -> Spec
testSlashes tz = it "parses description with slashes" $ do
    let source = [strT|
        BEGIN:VEVENT
        DTSTART:20151203T170000Z
        DTEND:20151203T180000Z
        DTSTAMP:20151127T150612Z
        ORGANIZER;CN=unknownorganizer@calendar.google.com:mailto:unknownorganizer@c
         alendar.google.com
        UID:7kukuqrfedlm2f9tpu19cgijm5bor4c0t814jvcmak9669k42o97ore4arbku72f2t10
        ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Emmanu
         el Touzery;X-NUM-GUESTS=0:mailto:etouzery@gmail.com
        CLASS:PRIVATE
        CREATED:20151124T184939Z
        DESCRIPTION:Za samodejno ustvarjene dogodke\, kot je ta\, si lahko podrobne
          podatke ogledate tako\, da uporabite uradno aplikacijo Google Koledar. htt
         p://g.co/calendar\n\nTa dogodek je bil ustvarjen iz e-poštnega sporočila\,
         ki ste ga prejeli v Gmailu. https://mail.google.com/mail?extsrc=cal&plid=AC
         UX6DMQjasq0BPzJxSSXD1TYiuCQZxgmyyP4JA\n
        LAST-MODIFIED:20151124T191516Z
        LOCATION:Kreativni center Poligon\, Tobačna ulica 5\, Ljubljana\, Ljubljana
         \, si\, 1000
        SEQUENCE:0
        STATUS:CONFIRMED
        SUMMARY:First Lambda Meetup!
        TRANSP:TRANSPARENT
        END:VEVENT
            |]
    let expected = TsEvent {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicons-46-calendar",
            eventDate = UTCTime (fromGregorian 2015 12 3)
                (secondsToDiffTime $ 17*3600),
            desc = "Za samodejno ustvarjene dogodke, kot je ta, si lahko podrobne podatke ogledate tako, da uporabite uradno aplikacijo Google Koledar. http://g.co/calendar\n\nTa dogodek je bil ustvarjen iz e-po\353tnega sporo\269ila,ki ste ga prejeli v Gmailu. https://mail.google.com/mail?extsrc=cal&plid=ACUX6DMQjasq0BPzJxSSXD1TYiuCQZxgmyyP4JA\nFirst Lambda Meetup!",
            extraInfo = "End: 18:00; duration: 1:00",
            fullContents = Nothing
        }
    testIcalParse tz source parseEvents [expected]

testIcalParse :: TimeZone -> Text
        -> T.Parsec Text () [Map String CalendarValue] -> [TsEvent] -> Assertion
testIcalParse tz = testParsecExpectTransform (concatMap (keyValuesToEvents tz))
