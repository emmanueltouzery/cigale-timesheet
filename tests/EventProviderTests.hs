{-# LANGUAGE TemplateHaskell #-}
module EventProviderTests (runEventProviderTests) where

import Test.Hspec
import Test.HUnit
import qualified Data.Text as T

import EventProvider

runEventProviderTests :: Spec
runEventProviderTests = do
	testThGetTypeDesc
	testThGetTypeDescComplex
	testThGetTypeDescComplexT

data TestData = TestData
	{
		field :: String,
		field1 :: Int
	}

testThGetTypeDesc :: Spec
testThGetTypeDesc = it "parses a simple type" $ do
	$(thGetTypeDesc ''TestData) `shouldBe` ["TestData", "field :: String", "field1 :: Int"]

data EmailConfigRecord = EmailRecord
	{
		emailProj :: T.Text,
		emailPatterns :: [T.Text]
	} deriving Show

data EmailConfig = EmailConfig
	{
		emailPaths :: [String],
		emailRecords :: [EmailConfigRecord]
		
	} deriving Show

testThGetTypeDescComplex :: Spec
testThGetTypeDescComplex = it "parses a complex type" $ do
	$(thGetTypeDesc ''EmailConfig) `shouldBe` ["EmailConfig", "emailPaths :: [String]", "emailRecords :: [EmailConfigRecord]"]

testThGetTypeDescComplexT :: Spec
testThGetTypeDescComplexT = it "parses a complex type text" $ do
	$(thGetTypeDesc ''EmailConfigRecord) `shouldBe` ["EmailConfigRecord", "emailProj :: Text", "emailPatterns :: [Text]"]
