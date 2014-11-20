{-# LANGUAGE OverloadedStrings #-}

module RedmineTests (runRedmineTests) where

import Test.Hspec

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import qualified Data.Text as T
import Event
import EventProvider

import Redmine

runRedmineTests :: Spec
runRedmineTests = do
	it "works ok with empty" $
		mergeSuccessiveEvents [] `shouldBe` []

	it "works ok with single element" $
		mergeSuccessiveEvents [eventWithDesc "a"] `shouldBe` [eventWithDesc "a"]

	it "does merge" $
		mergeSuccessiveEvents [eventWithDesc "a", eventWithDesc "a"] `shouldBe` [eventWithDesc "a"]

	it "does not merge too much" $
		mergeSuccessiveEvents [eventWithDesc "a", eventWithDesc "b"]
			`shouldBe` [eventWithDesc "a", eventWithDesc "b"]

	it "does merge also if titles differ a bit" $
		mergeSuccessiveEvents [eventWithDesc "a (more)", eventWithDesc "a (extra)"]
			`shouldBe` [eventWithDesc "a (more)"]

	let day = fromGregorian 2014 8 21

	it "parses morning time" $ parseTimeOfDay day "12:06 am" `shouldBe`
		LocalTime day (TimeOfDay 0 6 0)

	it "parses afternoon time" $ parseTimeOfDay day "02:28 pm" `shouldBe`
		LocalTime day (TimeOfDay 14 28 0)


eventWithDesc :: T.Text -> Event
eventWithDesc descVal = Event
	{
		pluginName = getModuleName getRedmineProvider,
		eventIcon = "glyphicon-tasks",
		eventDate = UTCTime (fromGregorian 2012 4 23) 0,
		desc = descVal,
		extraInfo = "",
		fullContents = Nothing
	}
