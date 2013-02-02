{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Email where

import Codec.Mbox
import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding 
import qualified Data.Text.ICU.Convert as ICU
import Text.ParserCombinators.Parsec
import Data.Text.Read
import GHC.Word

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
	parsedEmails <- sequence $ map parseMessage (mboxMessages mbox)
	let messages = takeWhile isAfter parsedEmails
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	--print messages1
	--B.putStrLn $ (headerVal "To: ") $ 
	return messages1
	where
		isAfter email = (localDay $ date email) >= fromDate
		isBefore email = (localDay $ date email) <= toDate

parseMessage :: MboxMessage BL.ByteString -> IO Email
parseMessage msg = do
	toVal <- headerValSafe "To: " msg
	ccVal <- headerVal "CC: " msg
	subjectVal <- headerValSafe "Subject: " msg
	let emailDate = parseEmailDate $ Util.toStrict1 $ _mboxMsgTime msg
	return $ Email emailDate toVal ccVal subjectVal

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
parseEmailDate v@_  = error $ "Invalid date format " ++ (T.unpack $ decodeUtf8 v)

decodeMime :: B.ByteString -> IO T.Text
decodeMime [brex|=\?(?{T.unpack . decodeUtf8 -> encoding}[\w\d-]+)
		\?Q\?(?{decodeUtf8 -> contents}.*)\?=|] = do
		conv <- ICU.open encoding Nothing
		let result = decodeMimeContents conv (T.unpack contents)
		return $ T.pack result
decodeMime s@_ = return $ decodeUtf8 s

decodeMimeContents :: ICU.Converter -> String -> String
decodeMimeContents conv contents = 
		case parseQuotedPrintable contents of
			Left _ -> concat ["can't parse ", contents , " as quoted printable?"]
			Right elts -> concatMap (qpEltToString conv) elts

qpEltToString :: ICU.Converter -> QuotedPrintableElement -> String
qpEltToString conv (AsciiSection str) = str
qpEltToString conv (NonAsciiChar chrInt) = T.unpack $ ICU.toUnicode conv $ BS.pack [chrInt]

data QuotedPrintableElement = AsciiSection String | NonAsciiChar Word8
	deriving (Show, Eq)

parseQuotedPrintable :: String -> Either ParseError [QuotedPrintableElement]
parseQuotedPrintable input = parse parseQPElements "" input

parseQPElements :: GenParser Char st [QuotedPrintableElement]
parseQPElements = do
	many $ parseAsciiSection <|> parseNonAsciiChar <|> parseUnderscoreSpace

parseAsciiSection :: GenParser Char st QuotedPrintableElement
parseAsciiSection = do
	contents <- many1 $ noneOf "=_"
	return $ AsciiSection contents

parseNonAsciiChar :: GenParser Char st QuotedPrintableElement
parseNonAsciiChar = do
	char '='
	value <- count 2 (oneOf "0123456789ABCDEFabcdef")
	case hexadecimal $ T.pack value of
		Right (a, _) -> return $ NonAsciiChar a
		_ -> return $ AsciiSection $ "internal error with hex string " ++ value

parseUnderscoreSpace :: GenParser Char st QuotedPrintableElement
parseUnderscoreSpace = do
	char '_'
	return $ AsciiSection " "

-- TODO this one is a bit over my head.
-- as input i have a Maybe (first monad)
-- and then I want to apply to it a monadic function
-- of the IO monad.
-- I first get rid of the Maybe.
headerVal :: B.ByteString -> MboxMessage BL.ByteString -> IO (Maybe T.Text)
headerVal header msg = case maybeRow of
			Nothing -> return Nothing
			Just x -> do
				text <- (decodeMime . (B.drop (B.length header))) x
				return $ Just text
	where
		msgContents = Util.toStrict1 $ _mboxMsgBody msg
		maybeRow = find (B.isPrefixOf $ header) (B.lines msgContents)

headerValSafe :: B.ByteString -> MboxMessage BL.ByteString -> IO T.Text
headerValSafe fieldHeader msg = do
		headerValV <- headerVal fieldHeader msg
		case headerValV of
			Just a -> return a
			Nothing -> return $ "Missing"
