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
import qualified Data.Aeson as Aeson
import qualified Codec.Text.IConv as IConv
import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Arrow ( (***) )
import Control.Error
import Network.HTTP.Types.URI (urlEncode)
import Data.Char (chr)
import Text.Printf
import Control.Monad.Trans

import Text.Regex.PCRE.Rex

import qualified Event
import EventProvider
import Util

data EmailConfig = EmailConfig
	{
		emailPath :: FilePath
		
	} deriving Show
deriveJSON defaultOptions ''EmailConfig

data AttachmentKey = AttachmentKey
	{
		mailId :: String,
		attachmentIndex :: Int
	} deriving Show
deriveJSON defaultOptions ''AttachmentKey

getEmailProvider :: EventProvider EmailConfig AttachmentKey
getEmailProvider = EventProvider
	{
		getModuleName = "Email",
		getEvents = getEmailEvents,
		getConfigType = members $(thGetTypeDesc ''EmailConfig),
		getExtraData = Just getMailAttachment
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

getEmailEvents :: EmailConfig -> GlobalSettings -> Day
	-> (AttachmentKey -> Url) -> EitherT String IO [Event.Event]
getEmailEvents cfg@(EmailConfig mboxLocation) _ day getAttachUrl = do
	emails <- lift $ getEmails getAttachUrl mboxLocation day day 
	timezone <- lift $ getTimeZone (UTCTime day 8)
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

getEmails :: (AttachmentKey -> Url) -> String -> Day -> Day -> IO [Email]
getEmails getAttachUrl sent_mbox fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	-- going from the end, stop at the first message which
	-- is older than my start date.
	let messages = takeWhile isAfter (mboxMessages mbox)

	-- now we remove the messages that are newer than end date
	-- need to reverse messages because i'm reading from the end.
	let messages1 = takeWhile isBefore (reverse messages)
	return $ map (messageToEmail getAttachUrl) messages1
	where
		dateMatches predicate email = predicate $ localDay (getEmailDate email)
		isAfter = dateMatches (>= fromDate)
		isBefore = dateMatches (<= toDate)

messageToEmail :: (AttachmentKey -> Url) -> MboxMessage BL.ByteString -> Email
messageToEmail getAttachUrl msg = do
	let emailDate = getEmailDate msg
	let msgBody = BSL.toStrict $ _mboxMsgBody msg
	let (headers, rawMessage) = parseMessage msg
	let toVal = readHeader "To" headers
	let ccVal = Map.lookup "CC" headers
	let subjectVal = readHeader "Subject" headers
	let messageId = fromMaybe "" $ getMessageId msg
	let emailContents = case parseMultipartSections headers rawMessage of
			Just sections -> T.concat
				[fromMaybe "no contents!" $ sectionTextContent <$> sectionToConsider sections,
				"<hr/>", getAttachmentBar getAttachUrl messageId sections]
			Nothing -> parseTextPlain $ MultipartSection headers rawMessage
	Email emailDate toVal ccVal subjectVal emailContents
	where
		-- TODO ugly to re-encode in ByteString, now I do ByteString->Text->ByteString->Text
		-- pazi another top-level function is also named readHeader!!!
		readHeader hName = decodeMime . encodeUtf8 . Map.findWithDefault "missing" hName

getAttachmentBar :: (AttachmentKey -> Url) -> String -> [MultipartSection] -> T.Text
getAttachmentBar getAttachUrl messageId sections = foldr (\(section,idx) -> T.append (T.pack $
		formatAttachmentDisplay section $ link idx)) "" $ attachmentSections sections
	where link = getAttachUrl . AttachmentKey messageId

formatAttachmentDisplay :: MultipartSection -> String -> String
formatAttachmentDisplay section link
	| "image/" `T.isPrefixOf` sectionCType section = printf "<p><img width='70%%' src='%s'><br/>%s</p>" link name
	| otherwise = printf ("<p><a href='%s' class='btn btn-default' role='button'>"
			++ "<span class='glyphicon glyphicon-paperclip'></span>%s</a></p>") link name
	where name = getAttachmentName section

getAttachmentName :: MultipartSection -> String
getAttachmentName (sectionCTDisposition -> disp) = T.unpack $ fromMaybe "" $ headerGetComponent "filename" disp

-- filters attachment sections only. Gives the section index too.
attachmentSections :: [MultipartSection] -> [(MultipartSection, Int)]
attachmentSections sections = filter (("attachment" `T.isInfixOf`) . sectionCTDisposition . fst) $ zip sections [0..]

parseMultipartSections :: Map T.Text T.Text -> BSL.ByteString -> Maybe [MultipartSection]
parseMultipartSections headers rawMessage = do
	contentType <- Map.lookup "Content-Type" headers
	separator <- if "multipart/" `T.isInfixOf` contentType
		then Just $ getMultipartSeparator contentType
		else Nothing
	parseMultipartBody separator rawMessage

getMultipartSeparator :: T.Text -> T.Text
getMultipartSeparator = fromMaybe "" . headerGetComponent "boundary"

headerGetComponent :: T.Text -> T.Text -> Maybe T.Text
headerGetComponent _componentName headerValue = do
	let componentName = _componentName `T.append` "="
	subElt <- find (T.isPrefixOf componentName)
		(T.stripStart <$> T.splitOn ";" headerValue)
	return $ T.dropAround (=='"') (T.drop (T.length componentName) subElt)

parseMessage :: MboxMessage BL.ByteString -> (Map T.Text T.Text, BSL.ByteString)
parseMessage msg = Util.parsecError parseMessageParsec "Email.parseMessage error: "
		(_mboxMsgBody msg)

parseMessageParsec :: T.Parsec BSL.ByteString st (Map T.Text T.Text, BSL.ByteString)
parseMessageParsec = do
	headers <- readHeaders
	body <- many anyChar
	return (Map.fromList headers, BL.pack body)

parseTextPlain :: MultipartSection -> T.Text
parseTextPlain section = T.replace "\n" "\n<br/>" (sectionFormattedContent section)

--textAfterHeaders :: T.Text -> T.Text
--textAfterHeaders txt = snd $ T.breakOn "\n\n" $ T.replace "\r" "" txt

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate = parseEmailDate . BSL.toStrict . _mboxMsgTime

parseMultipartBody :: T.Text -> BSL.ByteString -> Maybe [MultipartSection]
parseMultipartBody separator = Util.parseMaybe (parseMultipartBodyParsec mimeSeparator)
	where mimeSeparator = T.concat ["--", separator]

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
		sectionHeaders :: Map T.Text T.Text,
		sectionContent :: BSL.ByteString
	} deriving Show

sectionTextContent :: MultipartSection -> T.Text
sectionTextContent section
	| "multipart/" `T.isInfixOf` sectionCType section = -- multipart/alternative or multipart/related
		fromMaybe (error "Email.multipartBody error")
			$ sectionTextContent <$> (parsedSections >>= sectionToConsider)
	| "text/plain" `T.isInfixOf` sectionCType section = parseTextPlain section
	| otherwise = sectionFormattedContent section
	where
		parsedSections = parseMultipartBody mimeSeparator (sectionContent section)
		mimeSeparator = getMultipartSeparator $ sectionCType section

sectionFormattedContent :: MultipartSection -> T.Text
sectionFormattedContent section
	| sectionCTTransferEnc section == "quoted-printable" =
		decodeMimeContents encoding (decodeUtf8 $ BSL.toStrict $ sectionContent section)
	| otherwise = iconvFuzzyText encoding (sectionContent section)
	where
		encoding = sectionCharset section

-- for saving the email. Could be text or binary.
sectionDecodedContents :: MultipartSection -> BS.ByteString
sectionDecodedContents section
	| "text/" `T.isPrefixOf` sectionCType section = encodeUtf8 $ sectionFormattedContent section
	| sectionCTTransferEnc section == "base64" = Base64.decodeLenient contents
	| otherwise = contents
	where contents = BSL.toStrict $ sectionContent section

charsetFromContentType :: T.Text -> String
charsetFromContentType ct = T.unpack $ Map.findWithDefault "utf-8" "charset" kvHash
	where
		kvHash = Map.fromList $ map (split2 "=" . T.strip) $
			filter (T.isInfixOf "=") $ T.splitOn ";" ct

sectionCType :: MultipartSection -> T.Text
sectionCType section = fromMaybe "" (sectionContentType section)

sectionCTTransferEnc :: MultipartSection -> T.Text
sectionCTTransferEnc section = fromMaybe "" (sectionContentTransferEncoding section)

sectionCTDisposition :: MultipartSection -> T.Text
sectionCTDisposition section = fromMaybe "" $ Map.lookup "Content-Disposition" $ sectionHeaders section

sectionCharset :: MultipartSection -> String
sectionCharset = charsetFromContentType . sectionCType

split2 :: T.Text -> T.Text -> (T.Text, T.Text)
split2 a b = (x, T.concat xs)
	where
		(x:xs) = T.splitOn a b

sectionContentType :: MultipartSection -> Maybe T.Text
sectionContentType = Map.lookup "Content-Type" . sectionHeaders

sectionContentTransferEncoding :: MultipartSection -> Maybe T.Text
sectionContentTransferEncoding = Map.lookup "Content-Transfer-Encoding" . sectionHeaders

parseMultipartSection :: T.Text -> T.Parsec BSL.ByteString st MultipartSection
parseMultipartSection mimeSeparator = do
	headers <- Map.fromList <$> readHeaders
	contents <- manyTill readLine (T.try $ T.string $ T.unpack mimeSeparator)
	many eol
	return $ MultipartSection headers (BSL.intercalate "\n" contents)

readHeaders :: T.Parsec BSL.ByteString st [(T.Text, T.Text)]
readHeaders = do
	val <- manyTill readHeader (T.try eol)
	return $ join (***) (decodeUtf8 . BSL.toStrict) <$> val

readHeader :: T.Parsec BSL.ByteString st (BSL.ByteString, BSL.ByteString)
readHeader = do
	key <- BL.pack <$> T.many (T.noneOf ":\n\r")
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
iconvFuzzyText encoding input = decodeUtf8 $ BSL.toStrict lbsResult
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

getMailAttachment :: EmailConfig -> GlobalSettings -> AttachmentKey -> IO (Maybe (ContentType, BS.ByteString))
getMailAttachment (EmailConfig mboxLocation) _ (AttachmentKey emailId idx) =
	extractMailAttachment emailId  idx <$> parseMboxFile Backward mboxLocation

getMessageId :: MboxMessage BL.ByteString -> Maybe String
getMessageId msg = T.unpack <$> Map.lookup "Message-ID" (fst $ parseMessage msg)

extractMailAttachment :: String -> Int -> Mbox BL.ByteString -> Maybe (ContentType, BS.ByteString)
extractMailAttachment emailId index mbox = do
	msg <- find ((==Just emailId) . getMessageId) $ mboxMessages mbox
	let (headers, rawMessage) = parseMessage msg
	section <- parseMultipartSections headers rawMessage >>= flip atMay index
	contentType <- sectionContentType section
	return (T.unpack contentType, sectionDecodedContents section)
