{-# LANGUAGE OverloadedStrings, TemplateHaskell, ViewPatterns #-}

module Email where

-- TODO all over the place here I'm parsing bytestrings
-- in String and then converting back to Text.
-- http://stackoverflow.com/questions/21620298
-- Should probably move to attoparsec.
-- Also some bits are really messy here.
-- And we're still using error here...

import Codec.Mbox
import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Read
import GHC.Word
import qualified Data.ByteString.Base64 as Base64
import Text.Parsec.ByteString
import qualified Text.Parsec.Text as TT
import Text.Parsec
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Codec.Text.IConv as IConv
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import qualified Control.Applicative as A
import Control.Arrow ( (***) )
import Control.Error
import Text.Printf
import Control.Monad.Trans

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
	-> (AttachmentKey -> Url) -> ExceptT String IO [Event.Event]
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
		readHeader hName = decodeMimeHeader . encodeUtf8 . Map.findWithDefault "missing" hName

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

parseMessageParsec :: Parsec BSL.ByteString st (Map T.Text T.Text, BSL.ByteString)
parseMessageParsec = do
	headers <- readHeaders
	body <- many anyChar
	return (Map.fromList headers, BL.pack body)

parseTextPlain :: MultipartSection -> T.Text
parseTextPlain section = T.replace "\n" "\n<br/>" (sectionFormattedContent section)

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate msg = parseEmailDate $ fromMaybe decodedMboxDate
		(Map.lookup "Date" $ fst $ parseMessage msg)
	where
		mboxDate = BSL.toStrict $ _mboxMsgTime msg
		decodedMboxDate = if mboxDate == ""
			then error $ "Can't find a date for email: " ++ show msg
			else decodeUtf8 mboxDate

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

parseMultipartBodyParsec :: T.Text -> Parsec BSL.ByteString st [MultipartSection]
parseMultipartBodyParsec mimeSeparator = do
	manyTill readLine (try $ string $ T.unpack mimeSeparator)
	readLine
	manyTill (parseMultipartSection mimeSeparator) (try sectionsEnd)

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

parseMultipartSection :: T.Text -> Parsec BSL.ByteString st MultipartSection
parseMultipartSection mimeSeparator = do
	headers <- Map.fromList <$> readHeaders
	contents <- manyTill readLine (try $ string $ T.unpack mimeSeparator)
	many eol
	return $ MultipartSection headers (BSL.intercalate "\n" contents)

readHeaders :: Parsec BSL.ByteString st [(T.Text, T.Text)]
readHeaders = do
	val <- manyTill readHeader (try eol)
	return $ join (***) (decodeUtf8 . BSL.toStrict) <$> val

readHeader :: Parsec BSL.ByteString st (BSL.ByteString, BSL.ByteString)
readHeader = do
	key <- BL.pack <$> many (noneOf ":\n\r")
	string ":"
	many $ string " "
	val <- readHeaderValue
	return (key, val)

readHeaderValue :: Parsec BSL.ByteString st BSL.ByteString
readHeaderValue = do
	val <- many $ noneOf "\r\n"
	eol
	rest <- (do many1 $ oneOf " \t"; v <- readHeaderValue; return $ BSL.concat [" ", v])
		<|> return ""
	return $ BSL.concat [BL.pack val, rest]

sectionsEnd :: Parsec BSL.ByteString st ()
sectionsEnd = string "--" >> (eol <|> eof)

readLine :: Parsec BSL.ByteString st BSL.ByteString
readLine = BL.pack <$> many (noneOf "\r\n") <* eol

eol :: Parsec BSL.ByteString st ()
eol = optional (string "\r") >> void (string "\n")

readT :: B.ByteString -> Int
readT = fst . fromJust . B.readInt

readTT :: B.ByteString -> Integer
readTT = fst . fromJust . B.readInteger

parseEmailDate :: T.Text -> LocalTime
parseEmailDate (T.unpack -> dt) = fromMaybe (error $ "Email: error parsing date: " ++ dt)
	$ (parseTime defaultTimeLocale "%b %d %T %Y" dt
		A.<|> (zonedTimeToLocalTime <$> parseTime defaultTimeLocale "%a, %d %b %Y %T %z" dt)
		A.<|> parseTime defaultTimeLocale "%a %b %d %T %Y" dt
		A.<|> parseTime defaultTimeLocale "%a %b %e %T %Y" dt
		A.<|> (zonedTimeToLocalTime <$> parseTime defaultTimeLocale "%a, %-d %b %Y %T %z (%Z)" dt))

decUtf8IgnErrors :: B.ByteString -> T.Text
decUtf8IgnErrors = decodeUtf8With (\str input -> Just ' ')

iconvFuzzyText :: String -> BL.ByteString -> T.Text
iconvFuzzyText encoding input = decodeUtf8 $ BSL.toStrict lbsResult
	where lbsResult = IConv.convertFuzzy IConv.Transliterate encoding "utf8" input

decodeMimeHeader :: B.ByteString -> T.Text
decodeMimeHeader t = case parse parseMime "" t of
	Left x -> T.pack $ "Error parsing " ++ T.unpack (decUtf8IgnErrors t) ++ " -- " ++ show x
	Right x -> x
	where parseMime = T.concat <$> many
		((try decodeEncoded)
		<|> (T.pack <$> string "=")
		<|> decodePlainText)

decodeEncoded :: Parsec B.ByteString st T.Text
decodeEncoded = do
	string "=?"
	encoding <- many (noneOf "?")
	char '?'
	decodeBase64 encoding <|> decodeQuotedPrintable encoding

decodePlainText :: Parsec B.ByteString st T.Text
decodePlainText = T.pack <$> many1 (noneOf "=")

decodeQuotedPrintable :: String -> Parsec B.ByteString st T.Text
decodeQuotedPrintable encoding = do
	string "Q?"
	contentsVal <- many $ noneOf "?"
	string "?="
	return $ decodeMimeContents encoding $ T.pack contentsVal

decodeBase64 :: String -> Parsec B.ByteString st T.Text
decodeBase64 encoding = do
	string "B?"
	contentsB64 <- many $ noneOf "?"
	string "?="
	let contentsBinary = BL.fromChunks [Base64.decodeLenient $ B.pack contentsB64]
	return $ iconvFuzzyText encoding contentsBinary

decodeMimeContents :: String -> T.Text -> T.Text
decodeMimeContents encoding contentsVal = case parseQuotedPrintable (encodeUtf8 $ stripCarriageReturns contentsVal) of
	Left err -> T.concat ["can't parse ", contentsVal ,
		" as quoted printable? ", T.pack $ show err]
	Right elts -> T.concat $ map (qpEltToString encoding) elts

stripCarriageReturns :: T.Text -> T.Text
stripCarriageReturns = T.replace "=\n" ""

qpEltToString :: String -> QuotedPrintableElement -> T.Text
qpEltToString encoding (AsciiSection str) = T.pack str
qpEltToString encoding (NonAsciiChars chrInt) = iconvFuzzyText encoding (BSL.pack chrInt)
qpEltToString _ LineBreak = T.pack ""

data QuotedPrintableElement = AsciiSection String
		| NonAsciiChars [Word8]
		| LineBreak
	deriving (Show, Eq)

parseQuotedPrintable :: BS.ByteString -> Either ParseError [QuotedPrintableElement]
parseQuotedPrintable = parse parseQPElements ""

parseQPElements :: GenParser Char st [QuotedPrintableElement]
parseQPElements = many $ parseAsciiSection <|> parseUnderscoreSpace <|> parseNonAsciiChars

parseAsciiSection :: GenParser Char st QuotedPrintableElement
parseAsciiSection = AsciiSection <$> many1 (noneOf "=_")

parseNonAsciiChars :: GenParser Char st QuotedPrintableElement
parseNonAsciiChars = NonAsciiChars <$> many1 (try parseNonAsciiChar)

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
