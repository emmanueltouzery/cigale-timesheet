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

data GitRecord = GitRecord
	{
		gitProj :: T.Text,
		gitUser :: T.Text,
		gitRepo :: T.Text
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

data SkypeConfig = SkypeConfig
	{
		skypeUsername :: String
	} deriving Show

data ActivityConfig = ActivityConfig
	{
		svn :: [SvnRecord],
		hg :: [HgRecord],
		git :: [GitRecord],
		email :: EmailConfig,
		ical :: [IcalRecord],
		skype :: SkypeConfig
	}
	deriving Show

deriveJSON id ''SvnRecord
deriveJSON id ''HgRecord
deriveJSON id ''GitRecord
deriveJSON id ''EmailConfig
deriveJSON id ''EmailRecord
deriveJSON id ''IcalRecord
deriveJSON id ''SkypeConfig
deriveJSON id ''ActivityConfig

readConfig :: IO (Maybe ActivityConfig)
readConfig = do
	input <- BL.readFile "src/config.json"
	return $ parseActivityRecords input
	

parseActivityRecords :: BL.ByteString -> Maybe ActivityConfig
parseActivityRecords = decode
