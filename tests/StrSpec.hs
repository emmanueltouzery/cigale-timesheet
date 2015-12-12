{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module StrSpec (spec) where

import Test.Hspec
import Test.HUnit

import Str

spec :: Spec
spec = do
    testStrT
    testStrMultiT
    testStrCrMultiT
    testStrCrMultiBlank

testStrT :: Spec
testStrT = it "works on basic Text" $ do
        let converted = [strT|
                    test string|]
        let expected = "test string"
        assertEqual "doesn't match" expected converted

testStrMultiT :: Spec
testStrMultiT = it "works on two line Text" $ do
        let converted = [strT|
                    test string
                     second line|]
        let expected = "test string\n second line"
        assertEqual "doesn't match" expected converted

testStrCrT :: Spec
testStrCrT = it "works on basic CR Text" $ do
        let converted = [strCrT|
                    test string|]
        let expected = "test string\n"
        assertEqual "doesn't match" expected converted

testStrCrMultiT :: Spec
testStrCrMultiT = it "works on two line CR Text" $ do
        let converted = [strCrT|
                    test string
                     second line|]
        let expected = "test string\n second line\n"
        assertEqual "doesn't match" expected converted

testStrCrMultiBlank :: Spec
testStrCrMultiBlank = it "works with blank lines" $ do
        let converted = [strCrT|
                    test string

                     second line|]
        let expected = "test string\n\n second line\n"
        assertEqual "doesn't match" expected converted
