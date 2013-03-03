{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

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
import qualified Data.ByteString.Base64 as Base64
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T

import Text.Regex.PCRE.Rex

import Util

data Email = Email
	{
		date :: LocalTime,
		to :: T.Text,
		cc :: Maybe T.Text,
		subject :: T.Text,
		contents :: T.Text
	}
	deriving (Eq, Show)

getEmails :: FilePath -> Day -> Day -> IO [Email]
getEmails sent_mbox fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	--print $ head $ (map _mboxMsgTime (mboxMessages mbox))
	--print $ head $ (map parseMessage (mboxMessages mbox))
	putStrLn "before sequence"
	-- going from the end, stop at the first message which
	-- is older than my start date.
	let messages = takeWhile isAfter (mboxMessages mbox)

	-- now we remove the messages that are newer than end date
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	--print $ map getEmailDate messages1
	mapM parseMessage messages1
	where
		isAfter email = (localDay $ getEmailDate email) >= fromDate
		isBefore email = (localDay $ getEmailDate email) <= toDate

parseMessage :: MboxMessage BL.ByteString -> IO Email
parseMessage msg = do
	toVal <- headerValSafe "To: " msg
	ccVal <- headerVal "CC: " msg
	subjectVal <- headerValSafe "Subject: " msg
	contentType <- headerVal "Content-Type" msg
	print contentType
	let isMultipart = case contentType of
		Nothing -> False
		Just x -> "multipart/alternative" `T.isInfixOf` x
	let emailDate = getEmailDate msg
	print emailDate
	putStrLn $ "is multipartX? " ++ show isMultipart
	let msgBody = Util.toStrict1 $ _mboxMsgBody msg
	-- TODO i may hit a base64 body. decodeMime would be a
	-- starting base then. Must check the content-typel
	let bodyContents = textAfterHeaders $ decUtf8IgnErrors msgBody
	emailContents <- if isMultipart
			then Util.parsecParse parseMultipartBody bodyContents
			else Util.parsecParse parseAsciiBody bodyContents
	return $ Email emailDate toVal ccVal subjectVal emailContents

textAfterHeaders :: T.Text -> T.Text
textAfterHeaders txt = snd $ T.breakOn "\n\n" $ T.replace "\r" "" txt

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate = parseEmailDate . Util.toStrict1 . _mboxMsgTime

parseMultipartBody :: T.GenParser st T.Text
parseMultipartBody = do
	many eol
	optional $ string "This is a multi-part message in MIME format.\n"
	mimeSeparator <- readLine
	firstPart <- manyTill readLine (T.try $ T.string $ T.unpack mimeSeparator)
	secondPart <- manyTill readLine (T.try $ string $ T.unpack mimeSeparator)
	return $ textAfterHeaders $ T.unlines $ secondPart

parseAsciiBody :: T.GenParser st T.Text
parseAsciiBody = do
	many eol
	fmap T.pack (many anyToken)

readLine :: T.GenParser st T.Text
readLine = do
	val <- many $ noneOf "\r\n"
	eol
	return $ T.pack val

eol :: T.GenParser st T.Text
eol = do
	optional $ string "\r"
	string "\n"
	return "\n"

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
parseEmailDate v@_  = error $ "Invalid date format " ++ (T.unpack $ decUtf8IgnErrors v)

decUtf8IgnErrors :: B.ByteString -> T.Text
decUtf8IgnErrors = decodeUtf8With (\str input -> Just ' ')

decodeMime :: B.ByteString -> IO T.Text
-- base64
decodeMime [brex|=\?(?{T.unpack . decUtf8IgnErrors -> encoding}[\w\d-]+)
		\?B\?(?{contentsB64}.*)\?=|] = do
		conv <- ICU.open encoding Nothing
		let contentsBinary = Base64.decodeLenient contentsB64
		return $ ICU.toUnicode conv contentsBinary
-- quoted printable
decodeMime [brex|=\?(?{T.unpack . decUtf8IgnErrors -> encoding}[\w\d-]+)
		\?Q\?(?{decUtf8IgnErrors -> contentsVal}.*)\?=|] = do
		conv <- ICU.open encoding Nothing
		let result = decodeMimeContents conv (T.unpack contentsVal)
		return $ T.pack result
decodeMime s@_ = return $ decUtf8IgnErrors s

decodeMimeContents :: ICU.Converter -> String -> String
decodeMimeContents conv contentsVal = 
		case parseQuotedPrintable contentsVal of
			Left _ -> concat ["can't parse ", contentsVal , " as quoted printable?"]
			Right elts -> concatMap (qpEltToString conv) elts

qpEltToString :: ICU.Converter -> QuotedPrintableElement -> String
qpEltToString conv (AsciiSection str) = str
qpEltToString conv (NonAsciiChar chrInt) = T.unpack $ ICU.toUnicode conv $ BS.pack [chrInt]

data QuotedPrintableElement = AsciiSection String | NonAsciiChar Word8
	deriving (Show, Eq)

parseQuotedPrintable :: String -> Either ParseError [QuotedPrintableElement]
parseQuotedPrintable = parse parseQPElements ""

parseQPElements :: GenParser Char st [QuotedPrintableElement]
parseQPElements = many $ parseAsciiSection <|> parseNonAsciiChar <|> parseUnderscoreSpace

parseAsciiSection :: GenParser Char st QuotedPrintableElement
parseAsciiSection = do
	contentsVal <- many1 $ noneOf "=_"
	return $ AsciiSection contentsVal

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
		maybeRow = find (B.isPrefixOf header) (B.lines msgContents)

headerValSafe :: B.ByteString -> MboxMessage BL.ByteString -> IO T.Text
headerValSafe fieldHeader msg = do
		headerValV <- headerVal fieldHeader msg
		case headerValV of
			Just a -> return a
			Nothing -> return "Missing"
