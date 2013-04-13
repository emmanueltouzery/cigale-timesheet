{-# LANGUAGE OverloadedStrings #-}

module GitTests where

import Test.Hspec
import Data.Either

import Git

runGitTests :: Spec
runGitTests = it "parses advanced case" $ do
			let source = "commit b1eec0f4d78dd42e95df85ea83ee5852eaff1d7f\
\Author: David <David@Emelianenko>\
\Date:   Mon Apr 8 18:50:43 2013 +0200\
\\
\ tms/src/main/webapp/js/lib/app/data-admin/users.js | 2 ++\
\ 1 file changed, 2 insertions(+)\
\\
\commit 82bba38439d472380e258275e529da2ca25e2016\
\Merge: f67c552 3fe4331\
\Author: doblak <darjan@gmail.com>\
\Date:   Mon Apr 8 18:44:14 2013 +0200\
\\
\    Merge branch 'master' of gitlab:tms"
			parseCommitsParsec source `shouldSatisfy` isRight
	where
		isRight = null . lefts . return
