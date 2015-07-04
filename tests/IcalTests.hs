{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module IcalTests where

import Test.Hspec
import Test.HUnit

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar


import Str
import Ical
import Util
import Event
import EventProvider

import TestUtil

runIcalTests :: Spec
runIcalTests = do
    let tz = utc
    testBasicEvent tz
    testThroughMidnightEvent tz
    testWholeDayEvent tz

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
    let expected = Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
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
    let expected = [Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 17)
                (secondsToDiffTime $ 23*3600+30*60),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 0:29",
            fullContents = Nothing
        }, Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
            eventDate = UTCTime (fromGregorian 2013 4 18)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 00:30; duration: 0:30",
            fullContents = Nothing
        }]
    testParsecExpectTransform (concatMap (keyValuesToEvents tz)) source parseEvents expected

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
    let expected = [Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
            eventDate = UTCTime (fromGregorian 2014 3 31)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }, Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
            eventDate = UTCTime (fromGregorian 2014 4 1)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }, Event {
            pluginName = getModuleName getIcalProvider,
            eventIcon = "glyphicon-calendar",
            eventDate = UTCTime (fromGregorian 2014 4 2)
                (secondsToDiffTime 0),
            desc = "spent a lot of time researching bus tables for position records",
            extraInfo = "End: 23:59; duration: 23:59",
            fullContents = Nothing
        }]
    testParsecExpectTransform (concatMap (keyValuesToEvents tz)) source parseEvents expected
