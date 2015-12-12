{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module SvnSpec (spec) where

import Test.Hspec
import Test.HUnit
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as T

import Str
import Svn
import TestUtil
import Util

spec :: Spec
spec = testBasicCommit

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
        let expected = Commit {
                date = LocalTime (fromGregorian 2012 11 15) (TimeOfDay 13 49 13),
                user = "emmanuelt",
                comment = "new icon from metrel.\n",
                commitFiles = [
                    "/EAndroid/trunk/res/drawable-hdpi/ic_launcher.png",
                    "/EAndroid/trunk/res/drawable-ldpi/ic_launcher.png",
                    "/EAndroid/trunk/res/drawable-mdpi/ic_launcher.png",
                    "/EAndroid/trunk/res/drawable-xhdpi/ic_launcher.png"]
            }
        testParsecExpectFirst source parseCommits expected
