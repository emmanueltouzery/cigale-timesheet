{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module GitTests where

import Test.Hspec
import Test.HUnit
import Data.Either
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as T

import Str
import Git
import Util
import TestUtil

runGitTests :: Spec
runGitTests = do
	testUsualCommit
	testMerge
	testCommitWithoutMessage

testMerge :: Spec
testMerge = it "parses merge commits" $ do
		let source = [strT|
				commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
				Merge: xxxx
				Author: David <t@a>
				Date:   Mon Apr 8 18:50:43 2013 +0200
				
				 Did merge.
				
				|]
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
				commitDesc = Just "Did merge.",
				commitFiles = [],
				commitAuthor = "David <t@a>",
				commitContents = "<pre></pre>"
			}
		testParsecExpect source parseCommitsParsec expected

testUsualCommit :: Spec
testUsualCommit = it "parses usual commits" $ do
		let source = [strT|
				commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
				Author: David <t@a>
				Date:   Mon Apr 8 18:50:43 2013 +0200
				
				 Did commit.
				
				 test/src/main/users.js | 2 ++
				 1 file changed, 2 insertions(+)
				
				|]
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
				commitDesc = Just "Did commit.",
				commitFiles = ["test/src/main/users.js"],
				commitAuthor = "David <t@a>",
				commitContents = "<pre>test/src/main/users.js | 2 ++</pre>"
			}
		testParsecExpect source parseCommitsParsec expected

testCommitWithoutMessage :: Spec
testCommitWithoutMessage = it "parses commits without message" $ do
		let source = [strT|
				commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
				Author: David <t@a>
				Date:   Mon Apr 8 18:50:43 2013 +0200
				
				 test/src/main/users.js | 2 ++
				 1 file changed, 2 insertions(+)
				
				|]
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
				commitDesc = Nothing,
				commitFiles = ["test/src/main/users.js"],
				commitAuthor = "David <t@a>",
				commitContents = "<pre>test/src/main/users.js | 2 ++</pre>"
			}
		testParsecExpect source parseCommitsParsec expected
