{-# LANGUAGE OverloadedStrings #-}

module GitTests where

import Test.Hspec
import Test.HUnit
import Data.Either
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as T

import Git
import Util

runGitTests :: Spec
runGitTests = do
	testMerge
	testCommitWithoutMessage

testGitParseExpect :: T.Text -> Commit -> Assertion
testGitParseExpect source expected = do
	case parseCommitsParsec source of
		(Right (x:[])) -> assertEqual "Parse succeeded, value match" expected x
		Left pe -> assertBool ("Parse failed" ++ displayErrors pe) False
		_ -> assertBool "Parse succeeded, wrong results count" False

testMerge :: Spec
testMerge = it "parses merge commits" $ do
		let source = "commit b1eec0f4d734432e434385ea83ee5852eaff1d7f\n\
\Merge: xxxx\n\
\Author: David <t@a>\n\
\Date:   Mon Apr 8 18:50:43 2013 +0200\n\
\\n\
\ Did merge.\n\
\\n"
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
				commitDesc = "Did merge.",
				commitFiles = [""],
				commitAuthor = "David <t@a>",
				commitContents = "<pre></pre>"
			}
		testGitParseExpect source expected

testCommitWithoutMessage :: Spec
testCommitWithoutMessage = it "parses commits without message" $ do
		let source = "commit b1eec0f4d734432e434385ea83ee5852eaff1d7f\n\
\Author: David <t@a>\n\
\Date:   Mon Apr 8 18:50:43 2013 +0200\n\
\\n\
\ test/src/main/users.js | 2 ++\n\
\ 1 file changed, 2 insertions(+)\n\
\\n"
		let expected = Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
				commitDesc = "",
				commitFiles = ["test/src/main/users.js"],
				commitAuthor = "David <t@a>",
				commitContents = " test/src/main/users.js | 2 ++\
\ 1 file changed, 2 insertions(+)"
			}
		testGitParseExpect source expected
