{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

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
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Read
import Data.String.Conversions
import GHC.Word
import qualified Data.ByteString.Base64 as Base64
import Text.Parsec.ByteString
import Text.Parsec
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Codec.Text.IConv as IConv
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Control.Applicative as A
import Control.Arrow ( (***) )
import Control.Error
import Text.Printf.TH
import Control.Monad.Trans

import TsEvent
import EventProvider
import EventProviderSettings
import Util hiding (eol)

deriveConfigRecord emailConfigDataType
deriveJSON defaultOptions ''EmailConfigRecord

data AttachmentKey = AttachmentKey
    {
        mailId          :: String,
        attachmentIndex :: Int
    } deriving Show
deriveJSON defaultOptions ''AttachmentKey

getEmailProvider :: EventProvider EmailConfigRecord AttachmentKey
getEmailProvider = EventProvider
    {
        getModuleName = "Email",
        getEvents     = getEmailEvents,
        getConfigType = members emailConfigDataType,
        getExtraData  = Just getMailAttachment,
        fetchFieldCts = Nothing
    }

data Email = Email
    {
        date     :: LocalTime,
        to       :: Text,
        cc       :: Maybe Text,
        subject  :: Text,
        contents :: Text
    }
    deriving (Eq, Show)

getEmailEvents :: EmailConfigRecord -> GlobalSettings -> Day
               -> (AttachmentKey -> Url) -> ExceptT String IO [TsEvent]
getEmailEvents (EmailConfigRecord mboxLocation) _ day getAttachUrl = do
    emails <- lift $ getEmails getAttachUrl mboxLocation day day
    timezone <- lift $ getTimeZone (UTCTime day 8)
    return $ map (toEvent timezone) emails

toEvent :: TimeZone -> Email -> TsEvent
toEvent timezone email = TsEvent
    {
        pluginName = getModuleName getEmailProvider,
        eventIcon = "glyphicons-11-envelope",
        eventDate = localTimeToUTC timezone (date email),
        desc = subject email,
        extraInfo = "to: " <> to email,
        fullContents = Just $ Util.linksForceNewWindow $ contents email
    }

getEmails :: (AttachmentKey -> Url) -> String -> Day -> Day -> IO [Email]
getEmails getAttachUrl sent_mbox fromDate toDate = do
    mbox <- parseMboxFile Backward sent_mbox
    -- going from the end, stop at the first message which
    -- is older than my start date. Take 10 days margins because
    -- there is no order guarantee in mbox files and I've had problems
    -- (for thunderbird, parsing the .msf MORK file should be the solution but...)
    let messages = takeWhile (isAfter $ addDays (-10) fromDate) (mboxMessages mbox)

    -- now we remove the messages that are newer than end date
    -- need to reverse messages because i'm reading from the end.
    let messages1 = takeWhile (isBefore toDate) (reverse messages)
    let messages2 = filter (isAfter fromDate) messages1
    return $ map (messageToEmail getAttachUrl) messages2
    where
      dateMatches comp dte email = localDay (getEmailDate email) `comp` dte
      isAfter = dateMatches (>=)
      isBefore = dateMatches (<=)

messageToEmail :: (AttachmentKey -> Url) -> MboxMessage BL.ByteString -> Email
messageToEmail getAttachUrl msg = do
    let emailDate = getEmailDate msg
    let (headers, rawMessage) = parseMessage msg
    let toVal = rHeader "To" headers
    let ccVal = Map.lookup "CC" headers
    let subjectVal = rHeader "Subject" headers
    let messageId = fromMaybe "" $ getMessageId msg
    let emailContents = case parseMultipartSections headers rawMessage of
            Just sections -> T.concat
                [fromMaybe "no contents!" $ sectionTextContent <$> sectionToConsider sections,
                "<hr/>", getAttachmentBar getAttachUrl messageId sections]
            Nothing -> parseTextPlain $ MultipartSection headers rawMessage
    Email emailDate toVal ccVal subjectVal emailContents
    where
        -- TODO ugly to re-encode in ByteString, now I do ByteString->Text->ByteString->Text
        rHeader hName = decodeMimeHeader . encodeUtf8 . Map.findWithDefault "missing" hName

getAttachmentBar :: (AttachmentKey -> Url) -> String -> [MultipartSection] -> Text
getAttachmentBar getAttachUrl messageId sections = foldr (\(section,idx) -> T.append (T.pack $
        formatAttachmentDisplay section $ link idx)) "" $ attachmentSections sections
    where link = getAttachUrl . AttachmentKey messageId

formatAttachmentDisplay :: MultipartSection -> String -> String
formatAttachmentDisplay section link
    | "image/" `T.isPrefixOf` sectionCType section = [s|<p><img width='70%%' src='%s'><br/>%s</p>|] link name
    | otherwise = [s|<p><a href='%s' class='btn btn-default' role='button'>|] link <>
                  [s|<span class='glyphicon glyphicon-paperclip'></span>%s</a></p>|] name
    where name = getAttachmentName section

getAttachmentName :: MultipartSection -> String
getAttachmentName (sectionCTDisposition -> disp) = T.unpack $ fromMaybe "" $ headerGetComponent "filename" disp

-- filters attachment sections only. Gives the section index too.
attachmentSections :: [MultipartSection] -> [(MultipartSection, Int)]
attachmentSections sections = filter (("attachment" `T.isInfixOf`) . sectionCTDisposition . fst) $ zip sections [0..]

parseMultipartSections :: Map Text Text -> BSL.ByteString -> Maybe [MultipartSection]
parseMultipartSections headers rawMessage = do
    contentType <- Map.lookup "Content-Type" headers
    separator <- if "multipart/" `T.isInfixOf` contentType
        then Just $ getMultipartSeparator contentType
        else Nothing
    parseMultipartBody separator rawMessage

getMultipartSeparator :: Text -> Text
getMultipartSeparator = fromMaybe "" . headerGetComponent "boundary"

headerGetComponent :: Text -> Text -> Maybe Text
headerGetComponent _componentName headerValue = do
    let componentName = _componentName <> "="
    subElt <- find (T.isPrefixOf componentName)
        (T.stripStart <$> T.splitOn ";" headerValue)
    return $ T.dropAround (=='"') (T.drop (T.length componentName) subElt)

parseMessage :: MboxMessage BL.ByteString -> (Map Text Text, BSL.ByteString)
parseMessage msg = Util.parsecError parseMessageParsec "Email.parseMessage error: "
        (_mboxMsgBody msg)

parseMessageParsec :: Parsec BSL.ByteString st (Map Text Text, BSL.ByteString)
parseMessageParsec = do
    headers <- readHeaders
    body <- many anyChar
    return (Map.fromList headers, BL.pack body)

parseTextPlain :: MultipartSection -> Text
parseTextPlain section = T.replace "\n" "\n<br/>" (sectionFormattedContent section)

getEmailDate :: MboxMessage BL.ByteString -> LocalTime
getEmailDate msg = parseEmailDate $ fromMaybe decodedMboxDate
        (Map.lookup "Date" $ fst $ parseMessage msg)
    where
        mboxDate = BSL.toStrict $ _mboxMsgTime msg
        decodedMboxDate = if mboxDate == ""
            then error $ "Can't find a date for email: " ++ show msg
            else decodeUtf8 mboxDate

parseMultipartBody :: Text -> BSL.ByteString -> Maybe [MultipartSection]
parseMultipartBody separator = Util.parseMaybe (parseMultipartBodyParsec mimeSeparator)
    where mimeSeparator = "--" <> separator

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

sectionForMimeType :: Text -> [(Maybe Text, MultipartSection)] -> Maybe MultipartSection
sectionForMimeType mType secsByCt = snd <$> find (keyContainsStr mType) secsByCt
    where
        keyContainsStr _ (Nothing, _)  = False
        keyContainsStr str (Just x, _) = str `T.isInfixOf` x

parseMultipartBodyParsec :: Text -> Parsec BSL.ByteString st [MultipartSection]
parseMultipartBodyParsec mimeSeparator = do
    manyTill readLine (try $ string $ T.unpack mimeSeparator)
    readLine
    manyTill (parseMultipartSection mimeSeparator) (try sectionsEnd)

data MultipartSection = MultipartSection
    {
        sectionHeaders :: Map Text Text,
        sectionContent :: BSL.ByteString
    } deriving Show

sectionTextContent :: MultipartSection -> Text
sectionTextContent section
    | "multipart/" `T.isInfixOf` sectionCType section = -- multipart/alternative or multipart/related
        fromMaybe (error "Email.multipartBody error")
            $ sectionTextContent <$> (parsedSections >>= sectionToConsider)
    | "text/plain" `T.isInfixOf` sectionCType section = parseTextPlain section
    | otherwise = sectionFormattedContent section
    where
        parsedSections = parseMultipartBody mimeSeparator (sectionContent section)
        mimeSeparator = getMultipartSeparator $ sectionCType section

sectionFormattedContent :: MultipartSection -> Text
sectionFormattedContent section
    | sectionCTTransferEnc section == "quoted-printable" =
        decodeMimeContents encoding (decodeUtf8 $ BSL.toStrict $ sectionContent section)
    | sectionCTTransferEnc section == "base64" =
          iconvFuzzyText encoding $ convertString $ Base64.decodeLenient $
          BSL.toStrict (sectionContent section)
    | otherwise = iconvFuzzyText encoding (sectionContent section)
    where
        encoding = sectionCharset section

-- for saving the email. Could be text or binary.
sectionDecodedContents :: MultipartSection -> BS.ByteString
sectionDecodedContents section
    | "text/" `T.isPrefixOf` sectionCType section = encodeUtf8 $ sectionFormattedContent section
    | sectionCTTransferEnc section == "base64" = Base64.decodeLenient cts
    | otherwise = cts
    where cts = BSL.toStrict $ sectionContent section

charsetFromContentType :: Text -> String
charsetFromContentType ct = T.unpack $ Map.findWithDefault "utf-8" "charset" kvHash
    where
        kvHash = Map.fromList $ map (split2 "=" . T.strip) $
            filter (T.isInfixOf "=") $ T.splitOn ";" ct

sectionCType :: MultipartSection -> Text
sectionCType section = fromMaybe "" (sectionContentType section)

sectionCTTransferEnc :: MultipartSection -> Text
sectionCTTransferEnc section = fromMaybe "" (sectionContentTransferEncoding section)

sectionCTDisposition :: MultipartSection -> Text
sectionCTDisposition section = fromMaybe "" $ Map.lookup "Content-Disposition" $ sectionHeaders section

sectionCharset :: MultipartSection -> String
sectionCharset = charsetFromContentType . sectionCType

split2 :: Text -> Text -> (Text, Text)
split2 a b = (x, T.concat xs)
    where
        (x:xs) = T.splitOn a b

sectionContentType :: MultipartSection -> Maybe Text
sectionContentType = Map.lookup "Content-Type" . sectionHeaders

sectionContentTransferEncoding :: MultipartSection -> Maybe Text
sectionContentTransferEncoding = Map.lookup "Content-Transfer-Encoding" . sectionHeaders

parseMultipartSection :: Text -> Parsec BSL.ByteString st MultipartSection
parseMultipartSection mimeSeparator = do
    headers <- Map.fromList <$> readHeaders
    sectionCts <- manyTill readLine (try $ string $ T.unpack mimeSeparator)
    many eol
    return $ MultipartSection headers (BSL.intercalate "\n" sectionCts)

readHeaders :: Parsec BSL.ByteString st [(Text, Text)]
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
    rest <- (many1 (oneOf " \t") >> (" " <>) <$> readHeaderValue) <|> return ""
    return (BL.pack val <> rest)

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

-- I guess i'll have to find a smarter library
-- to parse dates instead of keeping piling those up??
parseEmailDate :: Text -> LocalTime
parseEmailDate (T.unpack -> dt) = fromMaybe (error $ "Email: error parsing date: " ++ dt)
    $ (parseT "%b %d %T %Y" dt
        A.<|> parseT "%a, %d %b %Y %T %z" dt
        A.<|> parseT "%a, %d %b %Y %T %Z" dt
        A.<|> parseT "%a %b %d %T %Y" dt
        A.<|> parseT "%a %b %e %T %Y" dt
        A.<|> parseT "%a, %-d %b %Y %T %z (%Z)" dt)
        -- the next line is for "Wed, 1 ..." which I saw
        -- %e is day of month, space-padded to two chars, 1 - 31
        A.<|> parseT "%a,%e %b %Y %T %z" dt
    where parseT = parseTimeM False defaultTimeLocale

decUtf8IgnErrors :: B.ByteString -> Text
decUtf8IgnErrors = decodeUtf8With (\_ _ -> Just ' ')

iconvFuzzyText :: String -> BL.ByteString -> Text
iconvFuzzyText encoding input = decodeUtf8 $ BSL.toStrict lbsResult
    where lbsResult = IConv.convertFuzzy IConv.Transliterate encoding "utf8" input

decodeMimeHeader :: B.ByteString -> Text
decodeMimeHeader t = case parse parseMime "" t of
    Left x -> T.pack $ "Error parsing " ++ T.unpack (decUtf8IgnErrors t) ++ " -- " ++ show x
    Right x -> x
    where parseMime = T.concat <$> many (try decodeEncoded
                                         <|> (T.pack <$> string "=")
                                         <|> decodePlainText)

decodeEncoded :: Parsec B.ByteString st Text
decodeEncoded = do
    string "=?"
    encoding <- many (noneOf "?")
    char '?'
    decodeBase64 encoding <|> decodeQuotedPrintable encoding

decodePlainText :: Parsec B.ByteString st Text
decodePlainText = T.pack <$> many1 (noneOf "=")

decodeQuotedPrintable :: String -> Parsec B.ByteString st Text
decodeQuotedPrintable encoding = do
    string "Q?"
    contentsVal <- many $ noneOf "?"
    string "?="
    return $ decodeMimeContents encoding $ T.pack contentsVal

decodeBase64 :: String -> Parsec B.ByteString st Text
decodeBase64 encoding = do
    string "B?"
    contentsB64 <- many $ noneOf "?"
    string "?="
    let contentsBinary = BL.fromChunks [Base64.decodeLenient $ B.pack contentsB64]
    return $ iconvFuzzyText encoding contentsBinary

decodeMimeContents :: String -> Text -> Text
decodeMimeContents encoding contentsVal = case parseQuotedPrintable encoded of
    Left parseErr -> [st|can't parse %s as quoted printable? %s|] contentsVal (show parseErr)
    Right elts    -> T.concat $ map (qpEltToString encoding) elts
    where encoded = encodeUtf8 $ stripCarriageReturns contentsVal

stripCarriageReturns :: Text -> Text
stripCarriageReturns = T.replace "=\n" ""

qpEltToString :: String -> QuotedPrintableElement -> Text
qpEltToString _ (AsciiSection str) = T.pack str
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

getMailAttachment :: EmailConfigRecord -> GlobalSettings -> AttachmentKey
                  -> IO (Maybe (ContentType, BS.ByteString))
getMailAttachment (EmailConfigRecord mboxLocation) _ (AttachmentKey emailId idx) =
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
