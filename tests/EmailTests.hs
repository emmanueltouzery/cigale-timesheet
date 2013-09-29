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
	testMimeNonUtf
	testMimeMultibyte
	testMimeB64

testEmail1 :: Spec
testEmail1 = it "parses a simple email structure" $ do
	testParsecExpectVal "\n\nThis is a multi-part message in MIME format.\nseparator\nfirstpart\nseparator\nsecond part\n\nafter headers\nseparator" (parse parseMultipartBody "") "\n\nafter headers\n"

testMimeNonUtf :: Spec
testMimeNonUtf = it "parses simple quoted printable" $
	assertEqual "doesn't match" "RE: FW: import datoteke - ki so šle skozi" (decodeMime "=?iso-8859-2?Q?RE:_FW:_import_datoteke_-_ki_so_=B9le_skozi?=")

testMimeMultibyte :: Spec
testMimeMultibyte = it "parses multibyte quoted printable" $
	assertEqual "doesn't match" "Arrières plans" (decodeMime "=?utf-8?Q?Arri=C3=A8res_plans?=")

testMimeB64 :: Spec
testMimeB64 = it "parses base64 email subject" $
	assertEqual "doesn't match" "Prevent Foreclosure & Save Your Home " (decodeMime "=?iso-8859-1?B?UHJldmVudCBGb3JlY2xvc3VyZSAmIFNhdmUgWW91ciBIb21lIA=?=")
