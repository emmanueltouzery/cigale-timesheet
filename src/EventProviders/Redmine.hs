{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Redmine where

import Data.ByteString as BS (ByteString(..), concat)
import Data.ByteString.Char8 as Char8 (split, pack)
import Data.ByteString.Lazy (fromChunks)
import Network.Http.Client
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Text.Encoding as TE
import Text.Printf.TH
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import Data.List (find)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe
import Control.Applicative hiding ((<|>))
import Data.Text.Lazy (toStrict)
import Control.Error
import Control.Monad.Trans
import Text.ParserCombinators.Parsec
import Data.Monoid

import Text.XML (Node(..), elementAttributes)
import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Scraping
import Text.HTML.DOM (parseLBS)

import qualified Util
import TsEvent
import EventProvider

type Password = Text

data RedmineConfig = RedmineConfig
    {
        redmineUrl :: Text,
        redmineUsername :: Text,
        redmineUserDisplay :: Text,
        redminePassword :: Password
    } deriving Show
deriveJSON defaultOptions ''RedmineConfig

getRedmineProvider :: EventProvider RedmineConfig ()
getRedmineProvider = EventProvider
    {
        getModuleName = "Redmine",
        getEvents = getRedmineEvents,
        getConfigType = members $(thGetTypeDesc ''RedmineConfig),
        getExtraData = Nothing
    }

getRedmineEvents :: RedmineConfig -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getRedmineEvents config _ day _ = do
    let url = addProtocolIfNeeded $ appendIfNeeded "/" $ redmineUrl config
    cookie <- lift $ login url config
    cookieValues <- hoistEither $ note "invalid cookie format" $ cookie >>= headMay . split ';'
    let activityUrl = encodeUtf8 $ prepareActivityUrl url day
    response <- lift $ Util.http GET activityUrl "" concatHandler $
        setHeader "Cookie" cookieValues
    timezone <- lift $ getTimeZone (UTCTime day 8)
    today <- lift $ utctDay <$> getCurrentTime
    return $ mergeSuccessiveEvents $ getIssues config response day today timezone

mergeSuccessiveEvents :: [TsEvent] -> [TsEvent]
mergeSuccessiveEvents (x:xs) = x : mergeSuccessiveEvents (dropWhile firstPartMatches xs)
    where
        firstPartMatches y = firstPart y == firstPart x
        firstPart = head . T.splitOn "(" . desc
mergeSuccessiveEvents [] = []

prepareActivityUrl :: Text -> Day -> Text
prepareActivityUrl url day = T.concat [url, "/activity?show_wiki_edits=1&show_issues=1&from=", dayBeforeStr]
    where
        (y, m, d) = toGregorian $ addDays 1 day
        dayBeforeStr = [st|"%d-%02d-%02d"|] y m d

addProtocolIfNeeded :: Text -> Text
addProtocolIfNeeded val = if hasProtocol then val else "http://" <> val
    where
        hasProtocol = "http://" `T.isPrefixOf` lowerVal
            || "https://" `T.isPrefixOf` lowerVal
        lowerVal = T.toLower val

appendIfNeeded :: Text -> Text -> Text
appendIfNeeded postfix val
    | postfix `T.isSuffixOf` val = val
    | otherwise = val <> postfix

-- returns the cookie
login :: Text -> RedmineConfig -> IO (Maybe ByteString)
login url config = postForm
        (encodeUtf8 url <> "login")
        [("username", encodeUtf8 $ redmineUsername config),
            ("password", encodeUtf8 $ redminePassword config)]
        (\r _ -> return $ getHeader r "Set-Cookie")

getIssues :: RedmineConfig -> ByteString -> Day -> Day -> TimeZone -> [TsEvent]
getIssues config html day today timezone = case maybeDayNode of
        Nothing -> [] -- no events at all that day.
        Just dayNode -> getIssuesForDayNode config day timezone dayNode
    where
        doc = fromDocument $ parseLBS $ fromChunks [html]
        dayNodes = queryT [jq| div#content div#activity h3 |] doc
        maybeDayNode = find (isDayTitle day today) dayNodes

isDayTitle :: Day -> Day -> Cursor -> Bool
isDayTitle day today nod = dayTitle == toStrict (innerText (node nod))
    where
        (y, m, d) = toGregorian day
        dayTitle = if day == today
                then "Today"
                else [st|%02d/%02d/%4d|] m d y

getIssuesForDayNode :: RedmineConfig -> Day -> TimeZone -> Cursor -> [TsEvent]
getIssuesForDayNode config day timezone dayNode = parseBugNodes config day timezone bugNodes
    where
        bugNodes = filter (isElement . node) (child dlNode)
        dlNode = fromMaybe (error "can't find the DL node")
                (find (isElement . node) (following dayNode))

parseBugNodes :: RedmineConfig -> Day -> TimeZone -> [Cursor] -> [TsEvent]
parseBugNodes config day timezone (bugInfo:changeInfo:rest@_) =
        if authorName == redmineUserDisplay config
        then TsEvent
            {
                pluginName = getModuleName getRedmineProvider,
                eventIcon = "glyphicons-361-bug",
                desc = toStrict $ innerText linkNode,
                extraInfo =  changeInfo >@> [jq|span.description|],
                fullContents = fmap (\x -> T.concat ["<a href='",
                    redmineUrl config,
                    x, "' target='_blank'>More information</a>"]) (linkTarget linkNode),
                eventDate = localTimeToUTC timezone localTime
            } : parseBugNodes config day timezone rest
        else parseBugNodes config day timezone rest
    where
        linkNode = node . head $ queryT [jq|a|] bugInfo
        linkTarget (NodeElement elt) = Map.lookup "href" (elementAttributes elt)
        localTime = parseTimeOfDay day $ T.unpack $ bugInfo >@> [jq|span.time|]
        authorName = changeInfo >@> [jq|span.author a|]
        n >@> q = toStrict . innerText . node . head $ queryT q n
parseBugNodes _ _ _ [] = []
parseBugNodes _ _ _ [_] = error "parseBugNodes: invalid pattern!?"

parseTimeOfDay :: Day -> String -> LocalTime
parseTimeOfDay day time = LocalTime day $ fromMaybe (error $ "Can't parse time: " ++ time) parsed
    where parsed = parseTimeM False defaultTimeLocale "%I:%M %P" time

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False
