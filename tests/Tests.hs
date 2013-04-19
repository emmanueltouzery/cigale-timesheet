import Test.Hspec

import GitTests
import RedmineTests
import SvnTests

main :: IO ()
main = do
	hspec $ do
	describe "GIT tests" $ do
		runGitTests
	describe "redmine mergeSuccessiveEvents" runRedmineTests
	describe "SVN tests" runSvnTests
