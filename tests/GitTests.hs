{-# LANGUAGE OverloadedStrings #-}

module GitTests where

import Test.Hspec
import Data.Either

import Git

runGitTests :: Spec
runGitTests = it "parses advanced case" $ do
			let source = "commit b1eec0f4d734432e434385ea83ee5852eaff1d7f\
\Author: David <t@a>\
\Date:   Mon Apr 8 18:50:43 2013 +0200\
\\
\ test/src/main/users.js | 2 ++\
\ 1 file changed, 2 insertions(+)\
\\
\commit 82bba3843784348384258275e529d43434334016\
\Merge: f545552 3232331\
\Author: john <john@x.com>\
\Date:   Mon Apr 8 18:44:14 2013 +0200\
\\
\    Merge branch 'master' of git:test"
			parseCommitsParsec source `shouldSatisfy` isRight
	where
		isRight = null . lefts . return
