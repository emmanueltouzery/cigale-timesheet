module EventProviderTestsData where

import qualified Data.Text as T

data TestData = TestData
    {
        field :: String,
        field1 :: Int
    }

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
