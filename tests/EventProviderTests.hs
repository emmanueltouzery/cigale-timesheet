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
	$(thGetTypeDesc ''TestData) `shouldBe` ConfigDataType"TestData"
		[ConfigDataInfo "field" "String", ConfigDataInfo "field1" "Int"]

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
	$(thGetTypeDesc ''EmailConfig) `shouldBe` (ConfigDataType "EmailConfig" 
		[ConfigDataInfo "emailPaths" "[String]",
		ConfigDataInfo "emailRecords" "[EmailConfigRecord]"])

testThGetTypeDescComplexT :: Spec
testThGetTypeDescComplexT = it "parses a complex type text" $ do
	$(thGetTypeDesc ''EmailConfigRecord) `shouldBe` (ConfigDataType "EmailConfigRecord"
		[ConfigDataInfo "emailProj" "Text",
		ConfigDataInfo "emailPatterns" "[Text]"])
