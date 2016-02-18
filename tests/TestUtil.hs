{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module TestUtil where

import Data.Text (Text)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec as T
import Test.HUnit

testParsecExpectVal :: (Eq a, Show a) => Text
        -> T.Parsec Text () a -> a -> Assertion
testParsecExpectVal = testParsecExpectTransform id

testParsecExpectFirst :: (Eq a, Show a) => Text
        -> T.Parsec Text () [a] -> a -> Assertion
testParsecExpectFirst = testParsecExpectTransform head

testParsecExpectTransform :: (Eq b, Show b) => (a->b) -> Text
        -> T.Parsec Text () a -> b -> Assertion
testParsecExpectTransform trans source parser expected = case parse parser "" source of
        (Right x) -> assertEqual "Parse succeeded, value doesn't match"
            expected (trans x)
        Left pe -> assertBool ("Parse failed" ++ show pe) False
