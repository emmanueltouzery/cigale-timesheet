import Test.Hspec

import GitTests
import RedmineTests

main :: IO ()
main = do
	hspec $ do
	describe "GIT tests" $ do
		runGitTests
	describe "redmine mergeSuccessiveEvents" runRedmineTests
