{-# LANGUAGE TemplateHaskell #-}
module EventProviderSpec (spec) where

import Test.Hspec

import EventProvider
import EventProviderTestsData

spec :: Spec
spec = do
    testThGetTypeDesc
    testThGetTypeDescComplexT

testThGetTypeDesc :: Spec
testThGetTypeDesc = it "parses a simple type" $
    $(thGetTypeDesc ''TestData) `shouldBe` ConfigDataType"TestData"
        [ConfigDataInfo "field" MtText, ConfigDataInfo "field1" MtText]

testThGetTypeDescComplexT :: Spec
testThGetTypeDescComplexT = it "parses a complex type text" $
    $(thGetTypeDesc ''EmailConfigRecord) `shouldBe` ConfigDataType "EmailConfigRecord"
        [ConfigDataInfo "emailProj" MtText,
        ConfigDataInfo "emailPatterns" MtText]
