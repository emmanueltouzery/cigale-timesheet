{-# LANGUAGE OverloadedStrings #-}

module RedmineTests where

import Test.Hspec

import Data.Time.Clock
import Data.Time.Calendar

import qualified Data.Text as T
import Event

import Redmine

runRedmineTests :: Spec
runRedmineTests = do
	it "works ok with empty" $ do
		mergeSuccessiveEvents [] `shouldBe` []

	it "works ok with single element" $ do
		mergeSuccessiveEvents [eventWithDesc "a"] `shouldBe` [eventWithDesc "a"]

	it "does merge" $ do
		mergeSuccessiveEvents [eventWithDesc "a", eventWithDesc "a"] `shouldBe` [eventWithDesc "a"]

	it "does not merge too much" $ do
		mergeSuccessiveEvents [eventWithDesc "a", eventWithDesc "b"]
			`shouldBe` [eventWithDesc "a", eventWithDesc "b"]

	it "does merge also if titles differ a bit" $ do
		mergeSuccessiveEvents [eventWithDesc "a (more)", eventWithDesc "a (extra)"]
			`shouldBe` [eventWithDesc "a (more)"]


eventWithDesc :: T.Text -> Event
eventWithDesc descVal = Event
	{
		eventDate = UTCTime (fromGregorian 2012 4 23) 0,
		project = Nothing,
		desc = descVal,
		extraInfo = "",
		fullContents = Nothing
	}
