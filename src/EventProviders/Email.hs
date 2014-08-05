{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell #-}

module Email where

import Codec.Mbox
import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding 
import Text.ParserCombinators.Parsec
import Data.Text.Read
import GHC.Word
import qualified Data.ByteString.Base64 as Base64
import qualified Text.Parsec.ByteString as T
import qualified Text.Parsec.Text as TT
import qualified Text.Parsec as T
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Codec.Text.IConv as IConv
import Debug.Trace
import qualified Data.Map as Map
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )

import Text.Regex.PCRE.Rex

import qualified Event
import EventProvider
import Util

data EmailConfig = EmailConfig
	{
		emailPath :: FilePath
		
	} deriving Show
deriveJSON defaultOptions ''EmailConfig

getEmailProvider :: EventProvider EmailConfig
getEmailProvider = EventProvider
	{
		getModuleName = "Email",
		getEvents = getEmailEvents,
		getConfigType = members $(thGetTypeDesc ''EmailConfig)
	}

data Email = Email
	{
		date :: LocalTime,
		to :: T.Text,
		cc :: Maybe T.Text,
		subject :: T.Text,
		contents :: T.Text
	}
	deriving (Eq, Show)

getEmailEvents :: EmailConfig -> GlobalSettings -> Day -> IO [Event.Event]
getEmailEvents (EmailConfig mboxLocation) _ day = do
	emails <- getEmails mboxLocation day day 
	timezone <- getTimeZone (UTCTime day 8)
	return $ map (toEvent timezone) emails

toEvent :: TimeZone -> Email -> Event.Event
toEvent timezone email = Event.Event
	{
		Event.pluginName = getModuleName getEmailProvider,
		Event.eventIcon = "glyphicon-envelope",
		Event.eventDate = localTimeToUTC timezone (date email),
		Event.desc = subject email,
		Event.extraInfo = T.concat["to: ", to email],
		Event.fullContents = Just $ contents email
	}

getEmails :: String -> Day -> Day -> IO [Email]
getEmails sent_mbox fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	-- going from the end, stop at the first message which
	-- is older than my start date.
	let messages = takeWhile isAfter (mboxMessages mbox)

	-- now we remove the messages that are newer than end date
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	return $ map parseMessage messages1
	where
		isAfter email = localDay (getEmailDate email) >= fromDate
		isBefore email = localDay (getEmailDate email) <= toDate

parseMessage :: MboxMessage BL.ByteString -> Email
parseMessage msg = do
	let emailDate = getEmailDate msg
	let msgBody = (trace $ "Parsing email " ++ show emailDate)
		Util.toStrict1 $ _mboxMsgBody msg
	let (headers, rawMessage) = Util.parsecParse parseMessageParsec (_mboxMsgBody msg)
	let toVal = readHeader "To" headers
	let ccVal = Map.lookup "CC" headers
	let subjectVal = readHeader "Subject" headers
	let contentType = Map.lookup "Content-Type" headers
	let multipartSeparator = case contentType of
		Just x | "multipart/" `T.isInfixOf` x -> Just $ getMultipartSeparator x
		_ -> Nothing
	let emailContents = case multipartSeparator of
			Just separator -> parseMultipartBody separator rawMessage
			Nothing -> parseTextPlain $ MultipartSection (Map.toList headers) rawMessage
	Email emailDate toVal ccVal subjectVal emailContents
	where
		-- TODO ugly to re-encode in ByteString, now I do ByteString->Text->ByteString->Text
		-- pazi another top-level function is also named readHeader!!!
		readHeader hName = decodeMime . encodeUtf8 . Map.findWithDefault "missing" hName

getMultipartSeparator :: T.Text -> T.Text
getMultipartSeparator contentType = case find
			(T.isPrefixOf boundary)
			(fmap T.stripStart $ T.splitOn ";" contentType) of
		Just section -> T.dropAround (=='"') (T.drop (T.length boundary) section)
		Nothing -> error $ "Invalid multipart content-type: " ++ T.unpack contentType
	where boundary = "boundary="

parseMessageParsec :: T.Parsec BSL.ByteString st (Map.Map T.Text T.Text, BSL.ByteString)
parseMessageParsec = do
	headers <- readHeaders
	body <- many anyChar
	return (Map.fromList headers, BL.pack body)

parseTextPlain :: MultipartSection -> T.Text
parseTextPlain section = T.replace "\n" "\n<br/>" (sectionFormattedContent section)

--textAfterHeaders :: T.Text -> T.Text
--textAfterHeaders txt = snd $ T.breakOn "\n\n" $ T.replace "\r" "" txt

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate = parseEmailDate . Util.toStrict1 . _mboxMsgTime

parseMultipartBody :: T.Text -> BSL.ByteString -> T.Text
parseMultipartBody separator body = maybe "no contents!" sectionTextContent
		$ sectionToConsider sections
	where
		sections = Util.parsecParse (parseMultipartBodyParsec mimeSeparator) body
		mimeSeparator = T.concat ["--", separator]

-- pick a section containing text/html or as a second choice text/plain,
-- and final choice multipart/alternative
sectionToConsider :: [MultipartSection] -> Maybe MultipartSection
sectionToConsider sections =
		sectionForMimeType "text/html" sectionsByContentTypes
			-- multipart/alternative or multipart/related
			`mplus` sectionForMimeType "multipart/" sectionsByContentTypes
			`mplus` sectionForMimeType "text/plain" sectionsByContentTypes
	where
		sectionsByContentTypes = zip (fmap sectionContentType sections) sections

sectionForMimeType :: T.Text -> [(Maybe T.Text, MultipartSection)] -> Maybe MultipartSection
sectionForMimeType mType secsByCt = snd <$> find (keyContainsStr mType) secsByCt
	where
		keyContainsStr str (Nothing, _) = False
		keyContainsStr str (Just x, _) = str `T.isInfixOf` x

parseMultipartBodyParsec :: T.Text -> T.Parsec BSL.ByteString st [MultipartSection]
parseMultipartBodyParsec mimeSeparator = do
	manyTill readLine (T.try $ T.string $ T.unpack mimeSeparator)
	readLine
	manyTill (parseMultipartSection mimeSeparator) (T.try sectionsEnd)

data MultipartSection = MultipartSection
	{
		sectionHeaders :: [(T.Text, T.Text)],
		sectionContent :: BSL.ByteString
	} deriving Show

sectionTextContent :: MultipartSection -> T.Text
sectionTextContent section
	| "multipart/" `T.isInfixOf` sectionCType = -- multipart/alternative or multipart/related
		parseMultipartBody mimeSeparator (sectionContent section)
	| "text/plain" `T.isInfixOf` sectionCType = parseTextPlain section
	| otherwise = sectionFormattedContent section
	where
		sectionCType = fromMaybe "" (sectionContentType section)
		mimeSeparator = getMultipartSeparator sectionCType

sectionFormattedContent :: MultipartSection -> T.Text
sectionFormattedContent section
	| sectionCTTransferEnc == "quoted-printable" =
		decodeMimeContents encoding (decodeUtf8 $ toStrict1 $ sectionContent section)
	| otherwise = iconvFuzzyText encoding (sectionContent section)
	where
		sectionCTTransferEnc = fromMaybe "" (sectionContentTransferEncoding section)
		encoding = sectionCharset section

charsetFromContentType :: T.Text -> String
charsetFromContentType ct = T.unpack $ Map.findWithDefault "utf-8" "charset" kvHash
	where
		kvHash = Map.fromList $ map (split2 "=" . T.strip) $
			filter (T.isInfixOf "=") $ T.splitOn ";" ct

sectionCharset :: MultipartSection -> String
sectionCharset section = charsetFromContentType contentType
	where
		contentType = fromMaybe "" (sectionContentType section)

split2 :: T.Text -> T.Text -> (T.Text, T.Text)
split2 a b = (x, T.concat xs)
	where
		(x:xs) = T.splitOn a b

sectionContentType :: MultipartSection -> Maybe T.Text
sectionContentType = sectionHeaderValue "Content-Type"

sectionContentTransferEncoding :: MultipartSection -> Maybe T.Text
sectionContentTransferEncoding = sectionHeaderValue "Content-Transfer-Encoding"

sectionHeaderValue :: T.Text -> MultipartSection -> Maybe T.Text
sectionHeaderValue headerName (MultipartSection headers _) = fmap snd $ find ((==headerName) . fst) headers

parseMultipartSection :: T.Text -> T.Parsec BSL.ByteString st MultipartSection
parseMultipartSection mimeSeparator = do
	headers <- readHeaders
	contents <- manyTill readLine (T.try $ T.string $ T.unpack mimeSeparator)
	many eol
	return $ MultipartSection headers (BSL.intercalate "\n" contents)

readHeaders :: T.Parsec BSL.ByteString st [(T.Text, T.Text)]
readHeaders = do
	val <- manyTill readHeader (T.try eol)
	return $ map (\(a,b) -> (decodeUtf8 $ toStrict1 $ BL.pack a, decodeUtf8 $ toStrict1 b)) val

readHeader :: T.Parsec BSL.ByteString st (String, BSL.ByteString)
readHeader = do
	key <- T.many $ T.noneOf ":\n\r"
	T.string ":"
	many $ T.string " "
	val <- readHeaderValue
	return (key, val)

readHeaderValue :: T.Parsec BSL.ByteString st BSL.ByteString
readHeaderValue = do
	val <- T.many $ T.noneOf "\r\n"
	eol
	rest <- (do many1 $ oneOf " \t"; v <- readHeaderValue; return $ BSL.concat [" ", v])
		<|> return ""
	return $ BSL.concat [BL.pack val, rest]

sectionsEnd :: T.Parsec BSL.ByteString st ()
sectionsEnd = T.string "--" >> (eol <|> eof)

readLine :: T.Parsec BSL.ByteString st BSL.ByteString
readLine = BL.pack <$> T.many (noneOf "\r\n") <* eol

eol :: T.Parsec BSL.ByteString st ()
eol = optional (string "\r") >> void (string "\n")

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
			_ -> error $ "Unknown month " ++ B.unpack month
parseEmailDate v@_  = error $ "Invalid date format " ++ T.unpack (decUtf8IgnErrors v)

decUtf8IgnErrors :: B.ByteString -> T.Text
decUtf8IgnErrors = decodeUtf8With (\str input -> Just ' ')

iconvFuzzyText :: String -> BL.ByteString -> T.Text
iconvFuzzyText encoding input = decodeUtf8 $ toStrict1 lbsResult
	where lbsResult = IConv.convertFuzzy IConv.Transliterate encoding "utf8" input

decodeMime :: B.ByteString -> T.Text
-- base64
decodeMime [brex|=\?(?{T.unpack . decUtf8IgnErrors -> encoding}[\w\d-]+)
		\?B\?(?{contentsB64}.*)\?=|] = do
		let contentsBinary = BL.fromChunks [Base64.decodeLenient contentsB64]
		iconvFuzzyText encoding contentsBinary
-- quoted printable
decodeMime [brex|=\?(?{T.unpack . decUtf8IgnErrors -> encoding}[\w\d-]+)
		\?Q\?(?{decUtf8IgnErrors -> contentsVal}.*)\?=|] = decodeMimeContents encoding contentsVal
decodeMime s@_ = decUtf8IgnErrors s

decodeMimeContents :: String -> T.Text -> T.Text
decodeMimeContents encoding contentsVal = 
		case parseQuotedPrintable (T.unpack contentsVal) of
			Left err -> T.concat ["can't parse ", contentsVal ,
				" as quoted printable? ", T.pack $ show err]
			Right elts -> T.concat $ map (qpEltToString encoding) elts

qpEltToString :: String -> QuotedPrintableElement -> T.Text
qpEltToString encoding (AsciiSection str) = T.pack str
qpEltToString encoding (NonAsciiChars chrInt) = iconvFuzzyText encoding (BSL.pack chrInt)
qpEltToString _ LineBreak = T.pack ""

data QuotedPrintableElement = AsciiSection String
		| NonAsciiChars [Word8]
		| LineBreak
	deriving (Show, Eq)

parseQuotedPrintable :: String -> Either ParseError [QuotedPrintableElement]
parseQuotedPrintable = parse parseQPElements ""

parseQPElements :: GenParser Char st [QuotedPrintableElement]
parseQPElements = many $ parseAsciiSection <|> parseUnderscoreSpace <|> try parseNonAsciiChars <|> try pqLineBreak

pqLineBreak :: GenParser Char st QuotedPrintableElement
pqLineBreak = do
	string "="
	optional $ string "\r"
	string "\n"
	return LineBreak

parseAsciiSection :: GenParser Char st QuotedPrintableElement
parseAsciiSection = AsciiSection <$> many1 (noneOf "=_")

parseNonAsciiChars :: GenParser Char st QuotedPrintableElement
parseNonAsciiChars = NonAsciiChars <$> many1 parseNonAsciiChar

parseNonAsciiChar :: GenParser Char st Word8
parseNonAsciiChar = do
	char '='
	value <- count 2 (oneOf "0123456789ABCDEFabcdef")
	case hexadecimal $ T.pack value of
		Right (a, _) -> return a
		_ -> error $ "internal error with hex string " ++ value

parseUnderscoreSpace :: GenParser Char st QuotedPrintableElement
parseUnderscoreSpace = char '_' >> return (AsciiSection " ")
