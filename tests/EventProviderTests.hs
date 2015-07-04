{-# LANGUAGE TemplateHaskell #-}
module EventProviderTests (runEventProviderTests) where

import Test.Hspec
import Test.HUnit
import qualified Data.Text as T

import EventProvider
import EventProviderTestsData

runEventProviderTests :: Spec
runEventProviderTests = do
    testThGetTypeDesc
    testThGetTypeDescComplex
    testThGetTypeDescComplexT

testThGetTypeDesc :: Spec
testThGetTypeDesc = it "parses a simple type" $
    $(thGetTypeDesc ''TestData) `shouldBe` ConfigDataType"TestData"
        [ConfigDataInfo "field" "String", ConfigDataInfo "field1" "Int"]

testThGetTypeDescComplex :: Spec
testThGetTypeDescComplex = it "parses a complex type" $
    $(thGetTypeDesc ''EmailConfig) `shouldBe` ConfigDataType "EmailConfig"
        [ConfigDataInfo "emailPaths" "[String]",
        ConfigDataInfo "emailRecords" "[EmailConfigRecord]"]

testThGetTypeDescComplexT :: Spec
testThGetTypeDescComplexT = it "parses a complex type text" $
    $(thGetTypeDesc ''EmailConfigRecord) `shouldBe` ConfigDataType "EmailConfigRecord"
        [ConfigDataInfo "emailProj" "Text",
        ConfigDataInfo "emailPatterns" "[Text]"]
