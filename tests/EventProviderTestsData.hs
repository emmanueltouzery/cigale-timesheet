module EventProviderTestsData where

import Data.Text (Text)

data TestData = TestData
    {
        field  :: String,
        field1 :: Text
    }

data EmailConfigRecord = EmailRecord
    {
        emailProj     :: Text,
        emailPatterns :: Text
    } deriving Show
