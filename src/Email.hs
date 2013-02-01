{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Email where

import Codec.Mbox
import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding 

import Text.Regex.PCRE.Rex

import Util

data Email = Email
	{
		date :: LocalTime,
		to :: T.Text,
		cc :: Maybe T.Text,
		subject :: T.Text
	}
	deriving (Eq)

getEmails :: FilePath -> Day -> Day -> IO [Email]
getEmails sent_mbox fromDate toDate = do
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
		isAfter email = (localDay $ date email) >= fromDate
		isBefore email = (localDay $ date email) <= toDate

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

parseEmailDate :: B.ByteString -> LocalTime
parseEmailDate [brex|(?{month}\w+)\s+(?{readT -> day}\d+)\s+
		(?{readT -> hour}\d+):(?{readT -> mins}\d+):(?{readT -> sec}\d+)\s+
		(?{readTT -> year}\d+)|] =
	LocalTime (fromGregorian year monthI day) (TimeOfDay hour mins (fromIntegral sec))
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

headerVal :: B.ByteString -> MboxMessage BL.ByteString -> Maybe T.Text
headerVal header msg = fmap (decodeUtf8 . (B.drop (B.length header))) maybeRow
	where
		msgContents = Util.toStrict1 $ _mboxMsgBody msg
		maybeRow = find (B.isPrefixOf $ header) (B.lines msgContents)
