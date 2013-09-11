module TestUtil where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Test.Hspec
import Test.HUnit

import Util

testParsecExpectVal :: (Eq a, Show a) => T.Text 
		-> (T.Text -> Either ParseError a) -> a -> Assertion
testParsecExpectVal = testParsecExpectTransform id

testParsecExpectFirst :: (Eq a, Show a) => T.Text 
		-> (T.Text -> Either ParseError [a]) -> a -> Assertion
testParsecExpectFirst = testParsecExpectTransform head

testParsecExpectTransform :: (Eq b, Show b) => (a->b) -> T.Text 
		-> (T.Text -> Either ParseError a) -> b -> Assertion
testParsecExpectTransform trans source parser expected = do
	case parser source of
		(Right x) -> assertEqual "Parse succeeded, value doesn't match"
			expected (trans x)
		Left pe -> assertBool ("Parse failed" ++ displayErrors pe) False
