{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, ViewPatterns #-}

module Redmine where

import Data.ByteString as BS (ByteString(..), concat)
import Data.ByteString.Char8 as Char8 (split, pack)
import Data.ByteString.Lazy (fromChunks)
import System.IO.Streams (write)
import Network.Http.Client
import Data.Text as T (Text(..), splitOn, pack, unpack, span, take, drop, concat)
import qualified Data.Map as Map
import Data.Text.Read (decimal)
import Data.Text.Encoding as TE
import Text.Printf
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List (find)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe
import Control.Applicative
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)

import Text.XML (Node(..), elementAttributes)
import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Scraping
import Text.HTML.DOM (parseLBS)

import qualified Util
import Event
import EventProvider

import Text.Regex.PCRE.Rex

type Password = T.Text

data RedmineConfig = RedmineConfig
	{
		redmineUrl :: T.Text,
		redmineUsername :: T.Text,
		redmineUserDisplay :: T.Text,
		redminePassword :: Password
	} deriving Show
deriveJSON defaultOptions ''RedmineConfig

getRedmineProvider :: EventProvider RedmineConfig
getRedmineProvider = EventProvider
	{
		getModuleName = "Redmine",
		getEvents = getRedmineEvents,
		getConfigType = members $(thGetTypeDesc ''RedmineConfig)
	}

getRedmineEvents :: RedmineConfig -> GlobalSettings -> Day -> IO [Event]
getRedmineEvents config _ day = do
	maybeCookie <- login config
	let cookieValues = head $ split ';' $ fromJust maybeCookie
	let activityUrl = encodeUtf8 $ prepareActivityUrl config day
	response <- Util.http activityUrl "" concatHandler $ do
		http GET "/activity?show_wiki_edits=1&show_issues=1"
		setHeader "Cookie" cookieValues
	timezone <- getTimeZone (UTCTime day 8)
	today <- utctDay <$> getCurrentTime
	return $ mergeSuccessiveEvents $ getIssues config response day today timezone

mergeSuccessiveEvents :: [Event] -> [Event]
mergeSuccessiveEvents (x:xs) = x : mergeSuccessiveEvents (dropWhile firstPartMatches xs)
	where
		firstPartMatches y = firstPart y == firstPart x
		firstPart = head . T.splitOn "(" . desc
mergeSuccessiveEvents [] = []

-- mergeSuccessiveEvents :: [Event] -> [Event]
-- mergeSuccessiveEvents (x:y:xs) = if firstPart x == firstPart y
-- 		then x : mergeSuccessiveEvents xs
-- 		else x : (mergeSuccessiveEvents $ y :xs)
-- 	where firstPart = head . (T.splitOn "(") . desc
-- mergeSuccessiveEvents whatever@_ = whatever

prepareActivityUrl :: RedmineConfig -> Day -> T.Text
prepareActivityUrl config day = T.concat [redmineUrl config, "/activity?from=", dayBeforeStr]
	where
		dayBefore = addDays (-1) day
		(y, m, d) = toGregorian dayBefore
		dayBeforeStr = T.pack $ printf "%d-%02d-%02d" y m d

-- returns the cookie
login :: RedmineConfig -> IO (Maybe ByteString)
login config = postForm
		(BS.concat [encodeUtf8 $ redmineUrl config, "login"])
		[("username", encodeUtf8 $ redmineUsername config), 
			("password", encodeUtf8 $ redminePassword config)]
		(\r _ -> return $ getHeader r "Set-Cookie")

getIssues :: RedmineConfig -> ByteString -> Day -> Day -> TimeZone -> [Event]
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
		dayTitle = T.pack $ if day == today
				then "Today"
				else printf "%02d/%02d/%4d" m d y

getIssuesForDayNode :: RedmineConfig -> Day -> TimeZone -> Cursor -> [Event]
getIssuesForDayNode config day timezone dayNode = parseBugNodes config day timezone bugNodes
	where
		bugNodes = filter (isElement . node) (child dlNode)
		dlNode = fromMaybe (error "can't find the DL node")
				(find (isElement . node) (following dayNode))

parseBugNodes :: RedmineConfig -> Day -> TimeZone -> [Cursor] -> [Event]
parseBugNodes config day timezone (bugInfo:changeInfo:rest@_) =
		if authorName == redmineUserDisplay config
		then Event
			{
				pluginName = getModuleName getRedmineProvider,
				eventIcon = "glyphicon-tasks",
				desc = toStrict $ innerText linkNode,
				extraInfo =  bugComment,
				fullContents = fmap (\x -> T.concat ["<a href='",
					redmineUrl config,
					x, "'>More information</a>"]) (linkTarget linkNode),
				eventDate = localTimeToUTC timezone localTime
			} : parseBugNodes config day timezone rest
		else parseBugNodes config day timezone rest
	where
		linkNode = node . head $ queryT [jq|a|] bugInfo
		linkTarget (NodeElement elt) = Map.lookup "href" (elementAttributes elt)
		localTime = parseTimeOfDay day $ T.unpack timeOfDayStr
		timeOfDayStr = firstNodeInnerText [jq|span.time|] bugInfo
		bugComment = firstNodeInnerText [jq|span.description|] changeInfo
		authorName = firstNodeInnerText [jq|span.author a|] changeInfo
		firstNodeInnerText q n = toStrict . innerText . node . head $ queryT q n
parseBugNodes _ _ _ [] = []
parseBugNodes _ _ _ [_] = error "parseBugNodes: invalid pattern!?"

parseTimeOfDay :: Day -> String -> LocalTime
parseTimeOfDay day [rex|(?{read -> hours}\d+)
			:(?{read -> mins}\d+)\s*
			(?{ampm}am|pm)|] = LocalTime day (TimeOfDay hours24 mins 0)
	where
		hours24 = if (ampm == "pm") && (hours < 12)
			then hours + 12 else hours
parseTimeOfDay _ x@_ = error $ "can't parse date " ++ x

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False
