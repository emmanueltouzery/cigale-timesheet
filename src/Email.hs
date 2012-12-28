{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Email where

import Codec.Mbox
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding 

import Text.Regex.PCRE.Rex

import Util

sent_mbox :: FilePath
sent_mbox = "C:\\Users\\emmanuelto\\AppData\\Roaming\\Thunderbird\\Profiles\\k5eh13s1.newprofile_windows7\\Mail\\mail.regulussoft.com\\Sent"

data Email = Email
	{
		date :: UTCTime,
		to :: T.Text,
		cc :: Maybe T.Text,
		subject :: T.Text
	}
	deriving (Eq)

getEmails :: Day -> Day -> IO [Email]
getEmails fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	--print $ head $ (map _mboxMsgTime (mboxMessages mbox))
	--print $ head $ (map parseMessage (mboxMessages mbox))
	let messages = takeWhile isAfter (map parseMessage (mboxMessages mbox))
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	--print messages1
	--B.putStrLn $ (headerVal "To: ") $ 
	return messages1
	where
		isAfter email = (utctDay $ date email) >= fromDate
		isBefore email = (utctDay $ date email) <= toDate

parseMessage :: MboxMessage BL.ByteString -> Email
parseMessage msg = Email (parseEmailDate $ Util.toStrict1 $ _mboxMsgTime msg)
			(headerValSafe "To: " msg)
			(headerVal "CC: " msg)
			(headerValSafe "Subject: " msg)

headerValSafe :: B.ByteString -> MboxMessage BL.ByteString -> T.Text
headerValSafe fieldHeader msg = case (headerVal fieldHeader msg) of
	Just a -> a
	Nothing -> T.pack ("ERROR: " ++ (T.unpack $ decodeUtf8 fieldHeader) ++ " -- " ++ (T.unpack $ decodeUtf8 $ Util.toStrict1 $ _mboxMsgBody msg))

readT :: B.ByteString -> Int
readT = fst . fromJust . B.readInt

readTT :: B.ByteString -> Integer
readTT = fst . fromJust . B.readInteger

parseEmailDate :: B.ByteString -> UTCTime
parseEmailDate [brex|(?{month}\w+)\s+(?{readT -> day}\d+)\s+
		(?{readTT -> hour}\d+):(?{readTT -> mins}\d+):(?{readTT -> sec}\d+)\s+
		(?{readTT -> year}\d+)|] =
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
			_ -> error $ "Unknown month " ++ (B.unpack month)
		secOfDay = (hour*3600 + mins*60 + sec) :: Integer

headerVal :: B.ByteString -> MboxMessage BL.ByteString -> Maybe T.Text
headerVal header msg = fmap (decodeUtf8 . (B.drop (B.length header))) maybeRow
	where
		msgContents = Util.toStrict1 $ _mboxMsgBody msg
		maybeRow = find (B.isPrefixOf $ header) (B.lines msgContents)
