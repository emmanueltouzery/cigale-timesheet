{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module SvnTests where

import Test.Hspec
import Test.HUnit
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as T

import Str
import Svn
import Util

runSvnTests :: Spec
runSvnTests = do
	testBasicCommit

testSvnParseExpect :: T.Text -> Commit -> Assertion
testSvnParseExpect source expected = do
	case parseCommitsParsec source of
		(Right (x:[])) -> assertEqual "Parse succeeded, value doesn't match" expected x
		Left pe -> assertBool ("Parse failed" ++ displayErrors pe) False
		_ -> assertBool "Parse succeeded, wrong results count" False

testBasicCommit :: Spec
testBasicCommit = it "parses a basic SVN commit" $ do
		let source = [strT|
			------------------------------------------------------------------------
			r939 | emmanuelt | 2012-11-15 13:49:13 +0100 (Thu, 15 Nov 2012) | 1 line
			Changed paths:
			   M /EAndroid/trunk/res/drawable-hdpi/ic_launcher.png
			   M /EAndroid/trunk/res/drawable-ldpi/ic_launcher.png
			   M /EAndroid/trunk/res/drawable-mdpi/ic_launcher.png
			   M /EAndroid/trunk/res/drawable-xhdpi/ic_launcher.png
			
			new icon from metrel.
			------------------------------------------------------------------------|]
		let expected = Commit
			{
				date = LocalTime (fromGregorian 2012 11 15) (TimeOfDay 13 49 13),
				user = "emmanuelt",
				comment = "new icon from metrel.\n",
				commitFiles = [
					"/EAndroid/trunk/res/drawable-hdpi/ic_launcher.png",
					"/EAndroid/trunk/res/drawable-ldpi/ic_launcher.png",
					"/EAndroid/trunk/res/drawable-mdpi/ic_launcher.png",
					"/EAndroid/trunk/res/drawable-xhdpi/ic_launcher.png"]
			}
		testSvnParseExpect source expected 
