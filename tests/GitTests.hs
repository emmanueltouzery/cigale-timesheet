{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module GitTests (runGitTests) where

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
	testMultipleCommits
	testMultipleCommitsFirstIsMerge

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
				commitContents = "<pre></pre>",
				commitIsMerge = True
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
				commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
				commitIsMerge = False
			}
		testParsecExpect source parseCommitsParsec expected

testMultipleCommits :: Spec
testMultipleCommits = it "parses multiple commits" $ do
		let source = [strT|
				commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
				Author: David <t@a>
				Date:   Mon Apr 8 18:50:43 2013 +0200
				
				 Did commit.
				
				 test/src/main/users.js | 2 ++
				 1 file changed, 2 insertions(+)
				
				commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
				Author: Emm <t@a>
				Date:   Mon Apr 8 18:50:43 2013 +0200
				
				 Did commit.
				
				 test/src/main/users.js | 2 ++
				 1 file changed, 2 insertions(+)
				
				|]
		let expected = 
			[
				Commit
				{
					commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
					commitDesc = Just "Did commit.",
					commitFiles = ["test/src/main/users.js"],
					commitAuthor = "David <t@a>",
					commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
					commitIsMerge = False
				},
				Commit
				{
					commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
					commitDesc = Just "Did commit.",
					commitFiles = ["test/src/main/users.js"],
					commitAuthor = "Emm <t@a>",
					commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
					commitIsMerge = False
				}
			]
		testParsecExpectList source parseCommitsParsec expected

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
				commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
				commitIsMerge = False
			}
		testParsecExpect source parseCommitsParsec expected


testMultipleCommitsFirstIsMerge :: Spec
testMultipleCommitsFirstIsMerge = it "parses multiple commits first is merge" $ do
		let source = [strT|
				commit d764a4398424f8bd2f1c659212ce6e0af83f5848
				Merge: dbcc720 95c25ed
				Author: David B <david@b>
				Date:   Wed Apr 3 16:54:39 2013 +0200
				
				    Merge branch 'master'
				
				commit 647d915f18d241185cf44b020ac5f962990350aa
				Author: Emmanuel Touzery <etouzery@gmail.com>
				Date:   Wed Apr 3 16:17:50 2013 +0200
				
				    blabla
				
				 t/README.md | 2 +-
				 1 file changed, 1 insertion(+), 1 deletion(-)
				
				|]
		testParsecExpectList source parseCommitsParsec [
			Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 3) (TimeOfDay 16 54 39),
				commitDesc = Just "Merge branch 'master'",
				commitFiles = [],
				commitAuthor = "David B <david@b>",
				commitContents = "<pre></pre>",
				commitIsMerge = True
			},
			Commit
			{
				commitDate = LocalTime (fromGregorian 2013 4 3) (TimeOfDay 16 17 50),
				commitDesc = Just "blabla",
				commitFiles = ["t/README.md"],
				commitAuthor = "Emmanuel Touzery <etouzery@gmail.com>",
				commitContents = "<pre>t/README.md | 2 +-</pre>",
				commitIsMerge = False
			}
			]
