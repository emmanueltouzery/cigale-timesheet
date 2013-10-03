{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, DeriveGeneric, TemplateHaskell #-}

module Email where

import Codec.Mbox
import Control.Monad
import Data.Time.Calendar
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
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Data.Aeson.TH (deriveJSON)
import qualified Codec.Text.IConv as IConv
import Debug.Trace

import Text.Regex.PCRE.Rex

import qualified Event
import EventProvider
import Util

data EmailConfig = EmailConfig
	{
		emailPath :: FilePath
		
	} deriving Show
deriveJSON id ''EmailConfig

getEmailProvider :: EventProvider EmailConfig
getEmailProvider = EventProvider
	{
		getModuleName = "Email",
		getEvents = getEmailEvents,
		getConfigType = members $(thGetTypeDesc ''EmailConfig)
		--getConfigRequirements = SubElementArraySpec [
		--	SubElementArraySpec [StringFieldSpec "emailPath"],
		--	SubElementArraySpec [StringFieldSpec "project", SubElementArraySpec [StringFieldSpec "emailPatterns"]]]
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
	timezone <- getCurrentTimeZone
	return $ map (toEvent timezone) emails

toEvent :: TimeZone -> Email -> Event.Event
toEvent timezone email = Event.Event
			{
				Event.pluginName = getModuleName getEmailProvider,
				Event.eventDate = localTimeToUTC timezone (date email),
				Event.desc = subject email,
				Event.extraInfo = T.concat["to: ", to email],
				Event.fullContents = Just $ contents email
			}


getEmails :: String -> Day -> Day -> IO [Email]
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
	return $ map parseMessage messages1
	where
		isAfter email = (localDay $ getEmailDate email) >= fromDate
		isBefore email = (localDay $ getEmailDate email) <= toDate

parseMessage :: MboxMessage BL.ByteString -> Email
parseMessage msg = do
	let toVal = headerValSafe "To: " msg
	let ccVal = headerVal "CC: " msg
	let subjectVal = headerValSafe "Subject: " msg
	let contentType = headerVal "Content-Type" msg
	let isMultipart = case contentType of
		Nothing -> False
		Just x -> "multipart/" `T.isInfixOf` x
	let emailDate = getEmailDate msg
	let msgBody = (trace $ "Parsing email " ++ (show emailDate) ++ " " ++ (T.unpack subjectVal))
		Util.toStrict1 $ _mboxMsgBody msg
	-- TODO i may hit a base64 body. decodeMime would be a
	-- starting base then. Must check the content-typel
	let bodyContents = textAfterHeaders $ decUtf8IgnErrors msgBody
	let emailContents = if isMultipart
			then parseMultipartBody bodyContents
			else Util.parsecParse parseAsciiBody bodyContents
	Email emailDate toVal ccVal subjectVal emailContents

textAfterHeaders :: T.Text -> T.Text
textAfterHeaders txt = snd $ T.breakOn "\n\n" $ T.replace "\r" "" txt

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate = parseEmailDate . Util.toStrict1 . _mboxMsgTime

parseMultipartBody :: T.Text -> T.Text
parseMultipartBody body =
		case sectionToConsider sections of
			Nothing -> "no contents!"
			Just s -> sectionTextContent s
	where
		sections = Util.parsecParse parseMultipartBodyParsec body

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
sectionForMimeType mType secsByCt = liftM snd (find (keyContainsStr mType) secsByCt)
	where
		keyContainsStr str (Nothing, _) = False
		keyContainsStr str (Just x, _) = (T.isInfixOf str) $ x

parseMultipartBodyParsec :: T.GenParser st [MultipartSection]
parseMultipartBodyParsec = do
	many eol
	optional $ string "This is a multi-part message in MIME format.\n"
	mimeSeparator <- readLine
	manyTill (parseMultipartSection mimeSeparator) (T.try $ sectionsEnd)

data MultipartSection = MultipartSection
	{
		sectionHeaders :: [(T.Text, T.Text)],
		sectionContent :: T.Text
	} deriving Show

sectionTextContent :: MultipartSection -> T.Text
sectionTextContent section
	| "multipart/" `T.isInfixOf` sectionCType = -- multipart/alternative or multipart/related
		parseMultipartBody (sectionContent section)
	| otherwise = sectionContent section
	where
		sectionCType = fromMaybe "" (sectionContentType section)

sectionContentType :: MultipartSection -> Maybe T.Text
sectionContentType (MultipartSection headers _) = fmap snd $ find ((=="Content-Type") . fst) headers

parseMultipartSection :: T.Text -> T.GenParser st MultipartSection
parseMultipartSection mimeSeparator = do
	headers <- manyTill readHeader (T.try $ do eol)
	contents <- manyTill readLine (T.try $ T.string $ T.unpack mimeSeparator)
	many eol
	return $ MultipartSection headers  (T.unlines contents)

readHeader :: T.GenParser st (T.Text, T.Text)
readHeader = do
	key <- many $ T.noneOf ":\n\r"
	T.string ":"
	many $ T.string " "
	val <- readHeaderValue
	return (T.pack key, val)

readHeaderValue :: T.GenParser st T.Text
readHeaderValue = do
	val <- many $ noneOf "\r\n;"
	rest <- (do T.string ";"; many $ T.string " "; optional eol; readHeaderValue) <|> eol
	return $ T.concat [T.pack val, rest]

sectionsEnd :: T.GenParser st ()
sectionsEnd = do
	T.string "--"
	(eol >> return ()) <|> eof

parseAsciiBody :: T.GenParser st T.Text
parseAsciiBody = do
	many eol
	textLines <- many readLine
	return $ T.intercalate "<br/>" textLines

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
		\?Q\?(?{decUtf8IgnErrors -> contentsVal}.*)\?=|] = do
		decodeMimeContents encoding contentsVal
decodeMime s@_ = decUtf8IgnErrors s

decodeMimeContents :: String -> T.Text -> T.Text
decodeMimeContents encoding contentsVal = 
		case parseQuotedPrintable (T.unpack contentsVal) of
			Left _ -> T.concat ["can't parse ", contentsVal , " as quoted printable?"]
			Right elts -> T.concat $ map (qpEltToString encoding) elts

qpEltToString :: String -> QuotedPrintableElement -> T.Text
qpEltToString encoding (AsciiSection str) = T.pack str
qpEltToString encoding (NonAsciiChars chrInt) = iconvFuzzyText encoding (BSL.pack chrInt)

data QuotedPrintableElement = AsciiSection String | NonAsciiChars [Word8]
	deriving (Show, Eq)

parseQuotedPrintable :: String -> Either ParseError [QuotedPrintableElement]
parseQuotedPrintable = parse parseQPElements ""

parseQPElements :: GenParser Char st [QuotedPrintableElement]
parseQPElements = many $ parseAsciiSection <|> parseNonAsciiChars <|> parseUnderscoreSpace

parseAsciiSection :: GenParser Char st QuotedPrintableElement
parseAsciiSection = do
	contentsVal <- many1 $ noneOf "=_"
	return $ AsciiSection contentsVal

parseNonAsciiChars :: GenParser Char st QuotedPrintableElement
parseNonAsciiChars = do
	chars <- many1 parseNonAsciiChar
	return $ NonAsciiChars chars

parseNonAsciiChar :: GenParser Char st Word8
parseNonAsciiChar = do
	char '='
	value <- count 2 (oneOf "0123456789ABCDEFabcdef")
	case hexadecimal $ T.pack value of
		Right (a, _) -> return a
		_ -> error $ "internal error with hex string " ++ value

parseUnderscoreSpace :: GenParser Char st QuotedPrintableElement
parseUnderscoreSpace = do
	char '_'
	return $ AsciiSection " "

headerVal :: B.ByteString -> MboxMessage BL.ByteString -> Maybe T.Text
headerVal header msg = liftM doDecode maybeRow
	where
		doDecode = decodeMime . (B.drop (B.length header))
		msgContents = Util.toStrict1 $ _mboxMsgBody msg
		maybeRow = find (B.isPrefixOf header) (B.lines msgContents)

headerValSafe :: B.ByteString -> MboxMessage BL.ByteString -> T.Text
headerValSafe fieldHeader msg = fromMaybe "Missing" (headerVal fieldHeader msg)
