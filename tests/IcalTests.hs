{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module IcalTests where

import Test.Hspec
import Test.HUnit

import qualified Data.Text as T
import qualified Data.Map as Map
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
	testBasicEvent

testBasicEvent :: Spec
testBasicEvent = it "parses basic ICAL event" $ do
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
		SUMMARY:spent a lot of time researching bus tables for position records
		TRANSP:OPAQUE
		END:VEVENT
			|]
	let expected = Event
		{
			pluginName = getModuleName getIcalProvider,
			eventDate = UTCTime (fromGregorian 2013 4 17)
				(secondsToDiffTime $ 7*3600+30*60),
			desc = "spent a lot of time researching bus tables for position records",
			extraInfo = "End: 09:00; duration: 1:30",
			fullContents = Nothing
		}
	testParsecExpectTransform (keyValuesToEvent . head) source parseEventsParsec expected
