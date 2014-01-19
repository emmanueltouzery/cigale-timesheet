import Test.Hspec

import GitTests
import RedmineTests
import SvnTests
import IcalTests
import StrTests
import SkypeTests
import EventProviderTests
import EmailTests

main :: IO ()
main = hspec $ do
	describe "Email tests" runEmailTests
	describe "STR tests" runStrTests
	describe "GIT tests" runGitTests
	describe "redmine mergeSuccessiveEvents" runRedmineTests
	describe "SVN tests" runSvnTests
	describe "Ical tests" runIcalTests
	describe "Skype tests" runSkypeTests
	describe "Event provider tests" runEventProviderTests
