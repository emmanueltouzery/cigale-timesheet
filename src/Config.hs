{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Config where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data SvnRecord = SvnRecord
	{
		svnProj :: T.Text,
		svnUser :: T.Text,
		svnRepo :: T.Text
	} deriving Show

data HgRecord = HgRecord
	{
		hgProj :: T.Text,
		hgUser :: T.Text,
		hgRepo :: T.Text
	} deriving Show

data EmailRecord = EmailRecord
	{
		emailProj :: T.Text,
		emailPatterns :: [T.Text]
	} deriving Show

data EmailConfig = EmailConfig
	{
		emailPaths :: [T.Text],
		emailRecords :: [EmailRecord]
		
	} deriving Show

data IcalRecord = IcalRecord
	{
		icalUrl :: T.Text
	} deriving Show

data ActivityConfig = ActivityConfig
	{
		svn :: [SvnRecord],
		hg :: [HgRecord],
		email :: EmailConfig,
		ical :: [IcalRecord]
	}
	deriving Show

deriveJSON id ''SvnRecord
deriveJSON id ''HgRecord
deriveJSON id ''EmailConfig
deriveJSON id ''EmailRecord
deriveJSON id ''IcalRecord
deriveJSON id ''ActivityConfig

readConfig :: IO (Maybe ActivityConfig)
readConfig = do
	input <- BL.readFile "src/config.json"
	return $ parseActivityRecords input
	

parseActivityRecords :: BL.ByteString -> Maybe ActivityConfig
parseActivityRecords = decode
