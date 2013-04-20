{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module StrTests (runStrTests) where

import Test.Hspec
import Test.HUnit

import Str

runStrTests :: Spec
runStrTests = do
	testStrT
	testStrMultiT

testStrT :: Spec
testStrT = it "works on basic Text" $ do
		let converted = [strT|
					test string|]
		let expected = "test string"
		assertEqual "doesn't match" expected converted

testStrMultiT :: Spec
testStrMultiT = it "works on basic Text" $ do
		let converted = [strT|
					test string
					 second line|]
		let expected = "test string\n second line"
		assertEqual "doesn't match" expected converted

testStrCrT :: Spec
testStrCrT = it "works on basic Text" $ do
		let converted = [strCrT|
					test string|]
		let expected = "test string\n"
		assertEqual "doesn't match" expected converted

testStrCrMultiT :: Spec
testStrCrMultiT = it "works on basic Text" $ do
		let converted = [strCrT|
					test string
					 second line|]
		let expected = "test string\n second line\n"
		assertEqual "doesn't match" expected converted
