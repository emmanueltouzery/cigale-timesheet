{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module HgTests (runHgTests) where

import Test.Hspec
import Test.HUnit
import Data.Time.LocalTime
import Data.Time.Calendar

import Hg
import Str
import TestUtil

runHgTests :: Spec
runHgTests = do
	testUsualCommit

testUsualCommit :: Spec
testUsualCommit = it "parses usual commits" $ do
		let source = [strCrT|
			2012-12-17 17:24 +0100
			auth filtering
			--->>>
			_poc/helloplay/app/controllers/Global.scala _poc/helloplay/app/controllers/NeedSecured.scala _poc/helloplay/app/views/login.scala.html _poc/helloplay/conf/routes
			--->>>|]
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2012 12 17) (TimeOfDay 17 24 0),
				commitDesc = "auth filtering\n",
				commitFiles = ["_poc/helloplay/app/controllers/Global.scala", "_poc/helloplay/app/controllers/NeedSecured.scala", "_poc/helloplay/app/views/login.scala.html", "_poc/helloplay/conf/routes"]
			}
		testParsecExpect source parseCommitsParsec expected
