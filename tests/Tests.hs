import Test.Hspec

import GitTests
import RedmineTests
import SvnTests
import IcalTests

main :: IO ()
main = do
	hspec $ do
	describe "GIT tests" $ do
		runGitTests
	describe "redmine mergeSuccessiveEvents" runRedmineTests
	describe "SVN tests" runSvnTests
	describe "Ical tests" runIcalTests
