{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module TestUtil where

import qualified Data.Functor.Identity as DFI
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Test.Hspec
import Test.HUnit

import Util

testParsecExpectVal :: (T.Stream T.Text DFI.Identity t, Eq a, Show a) => T.Text 
		-> T.Parsec T.Text () a -> a -> Assertion
testParsecExpectVal = testParsecExpectTransform id

testParsecExpectFirst :: (T.Stream T.Text DFI.Identity t, Eq a, Show a) => T.Text 
		-> T.Parsec T.Text () [a] -> a -> Assertion
testParsecExpectFirst = testParsecExpectTransform head

testParsecExpectTransform :: (T.Stream T.Text DFI.Identity t, Eq b, Show b) => (a->b) -> T.Text 
		-> T.Parsec T.Text () a -> b -> Assertion
testParsecExpectTransform trans source parser expected = case parse parser "" source of
		(Right x) -> assertEqual "Parse succeeded, value doesn't match"
			expected (trans x)
		Left pe -> assertBool ("Parse failed" ++ show pe) False
