{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module TestUtil where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import qualified Text.Parsec as T
import Test.HUnit

testParsecExpectVal :: (Eq a, Show a) => T.Text
        -> T.Parsec T.Text () a -> a -> Assertion
testParsecExpectVal = testParsecExpectTransform id

testParsecExpectFirst :: (Eq a, Show a) => T.Text
        -> T.Parsec T.Text () [a] -> a -> Assertion
testParsecExpectFirst = testParsecExpectTransform head

testParsecExpectTransform :: (Eq b, Show b) => (a->b) -> T.Text
        -> T.Parsec T.Text () a -> b -> Assertion
testParsecExpectTransform trans source parser expected = case parse parser "" source of
        (Right x) -> assertEqual "Parse succeeded, value doesn't match"
            expected (trans x)
        Left pe -> assertBool ("Parse failed" ++ show pe) False
