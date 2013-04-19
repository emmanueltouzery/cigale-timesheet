module TestUtil where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Test.Hspec
import Test.HUnit

import Util

testParsecExpect :: (Eq a, Show a) => T.Text 
		-> (T.Text -> Either ParseError [a]) -> a -> Assertion
testParsecExpect source parser expected = do
	case parser source of
		(Right (x:[])) -> assertEqual "Parse succeeded, value doesn't match" expected x
		Left pe -> assertBool ("Parse failed" ++ displayErrors pe) False
		_ -> assertBool "Parse succeeded, wrong results count" False
