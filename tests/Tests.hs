{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Time.Clock

import qualified Data.Text as T
import Event
import Redmine

main :: IO ()
main = do
	now <- getCurrentTime
	hspec $ do
	describe "redmine mergeSuccessiveEvents" $ do
		it "ok with empty" $ do
			mergeSuccessiveEvents [] `shouldBe` []

		it "ok with single element" $ do
			mergeSuccessiveEvents [eventWithDesc "a" now] `shouldBe` [eventWithDesc "a" now]


eventWithDesc :: T.Text -> UTCTime -> Event
eventWithDesc descVal now = Event
	{
		eventDate = now,
		project = Nothing,
		desc = descVal,
		extraInfo = "",
		fullContents = Nothing
	}
