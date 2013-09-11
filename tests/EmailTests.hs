{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module EmailTests (runEmailTests) where

import Test.Hspec
import Test.HUnit

import Codec.Mbox
import Text.ParserCombinators.Parsec (parse)

import Str
import Util
import TestUtil

import Email

runEmailTests :: Spec
runEmailTests = do
	testEmail1

testEmail1 :: Spec
testEmail1 = it "parses a simple email structure" $ do
	testParsecExpectVal "\n\nThis is a multi-part message in MIME format.\nseparator\nfirstpart\nseparator\nsecond part\n\nafter headers\nseparator" (parse parseMultipartBody "") "\n\nafter headers\n"
