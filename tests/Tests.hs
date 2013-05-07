import Test.Hspec

import GitTests
import RedmineTests
import SvnTests
import IcalTests
import StrTests
import SkypeTests

main :: IO ()
main = do
	hspec $ do
	describe "STR tests" runStrTests
	describe "GIT tests" runGitTests
	describe "redmine mergeSuccessiveEvents" runRedmineTests
	describe "SVN tests" runSvnTests
	describe "Ical tests" runIcalTests
	describe "Skype tests" runSkypeTests
