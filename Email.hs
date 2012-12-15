{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Email where

import Codec.Mbox
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.Maybe
import Data.List
import Data.Time

import Text.Regex.PCRE.Rex

sent_mbox = "C:\\Users\\emmanuelto\\AppData\\Roaming\\Thunderbird\\Profiles\\k5eh13s1.newprofile_windows7\\Mail\\mail.regulussoft.com\\Sent"

data Email = Email
	{
		date :: UTCTime,
		to :: BS.ByteString
	}
	deriving (Eq, Show)

getEmails :: Day -> Day -> IO [Email]
getEmails fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	print $ head $ (map _mboxMsgTime (mboxMessages mbox))
	print $ head $ (map parseMessage (mboxMessages mbox))
	let messages = takeWhile isAfter (map parseMessage (mboxMessages mbox))
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	print messages1
	--BS.putStrLn $ (headerVal "To: ") $ 
	return []
	where
		isAfter email = (utctDay $ date email) >= fromDate
		isBefore email = (utctDay $ date email) <= toDate

parseMessage :: MboxMessage BS.ByteString -> Email
parseMessage msg = Email (parseEmailDate $ BS.unpack $ _mboxMsgTime msg) (headerVal "To: " msg)

-- TODO i'd rather this takes a byte string... should be possible with "brex"
parseEmailDate :: String -> UTCTime
parseEmailDate [rex|(?{month}\w+)\s+(?{read -> day}\d+)\s+
		(?{read -> hour}\d+):(?{read -> min}\d+):(?{read -> sec}\d+)\s+
		(?{read -> year}\d+)|] =
	UTCTime (fromGregorian year monthI day) (secondsToDiffTime secOfDay)
	where
		monthI = case month of
			"Jan" -> 1
			"Feb" -> 2
			"Mar" -> 3
			"Apr" -> 4
			"May" -> 5
			"Jun" -> 6
			"Jul" -> 7
			"Aug" -> 8
			"Sep" -> 9
			"Oct" -> 10
			"Nov" -> 11
			"Dec" -> 12
			otherwise -> error $ "Unknown month " ++ month
		secOfDay = (hour*3600 + min*60 + sec) :: Integer

headerVal :: BS.ByteString -> MboxMessage BS.ByteString -> BS.ByteString
headerVal header msg = BS.drop (BS.length header) $ fromJust maybeRow
	where maybeRow = find (BS.isPrefixOf $ header) (BS.lines $ _mboxMsgBody msg)
