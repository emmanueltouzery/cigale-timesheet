{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Redmine (getRedmineProvider) where

import Data.ByteString as BS (ByteString(..), concat)
import Data.ByteString.Char8 as Char8 (split, pack)
import Data.ByteString.Lazy (fromChunks)
import System.IO.Streams (write)
import Network.Http.Client
import Data.Maybe
import Data.Text as T (Text(..), splitOn, pack, span, take, drop)
import Data.Text.Read (decimal)
import Data.Text.Encoding as TE
import Text.Printf
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List (find)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe

import Text.XML (Node(..))
import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Scraping
import Text.HTML.DOM (parseLBS)

import qualified Util
import Event
import EventProvider

import Debug.Trace

data RedmineConfig = RedmineConfig
	{
		redmineUrl :: ByteString,
		redmineUsername :: ByteString,
		redmineUserDisplay :: T.Text,
		redminePassword :: ByteString
	} deriving Show
deriveJSON id ''RedmineConfig

getRedmineProvider :: EventProvider RedmineConfig
getRedmineProvider = EventProvider
	{
		getModuleName = "Redmine",
		getEvents = getRedmineEvents
	}

getRedmineEvents :: RedmineConfig -> Day -> IO [Event]
getRedmineEvents config day = do
	maybeCookie <- login config (redmineUsername config) (redminePassword config)
	let cookieRows = split '\n' $ fromJust maybeCookie
	let cookieValues = fmap (head . (split ';')) cookieRows
	let activityUrl = prepareActivityUrl day
	response <- Util.http activityUrl "" concatHandler $ do
		http GET "/activity"
		setHeader "Cookie" (cookieValues !! 1)
	timezone <- getCurrentTimeZone
	return $ getIssues config response day timezone

prepareActivityUrl :: Day -> ByteString
prepareActivityUrl day = BS.concat ["http://redmine/activity?from=", dayBeforeStr]
	where
		dayBefore = addDays (-1) day
		(y, m, d) = toGregorian dayBefore
		dayBeforeStr = Char8.pack $ printf "%d-%02d-%02d" y m d

-- returns the cookie
login :: RedmineConfig -> ByteString -> ByteString -> IO (Maybe ByteString)
login config username password = do
	postForm
		(BS.concat [redmineUrl config, "login"])
		[("username", redmineUsername config), ("password", redminePassword config)]
		(\r i -> return $ getHeader r "Set-Cookie")

getIssues :: RedmineConfig -> ByteString -> Day -> TimeZone -> [Event]
getIssues config html day timezone = getIssuesForDayNode config day timezone dayNode
	where
		doc = fromDocument $ parseLBS $ fromChunks [html]
		dayNodes = queryT [jq| div#content div#activity h3 |] doc
		dayNode = fromJust $ find (isDayTitle day) dayNodes

isDayTitle :: Day -> Cursor -> Bool
isDayTitle day nod = dayTitle == innerTextN (node nod)
	where
		(y, m, d) = toGregorian day
		dayTitle = T.pack $ printf "%02d/%02d/%4d" m d y

getIssuesForDayNode :: RedmineConfig -> Day -> TimeZone -> Cursor -> [Event]
getIssuesForDayNode config day timezone dayNode = parseBugNodes config day timezone bugNodes
	where
		bugNodes = filter (isElement . node) (child dlNode)
		(Just dlNode) = find (isElement . node) (following dayNode)

parseBugNodes :: RedmineConfig -> Day -> TimeZone -> [Cursor] -> [Event]
parseBugNodes config day timezone (bugInfo:changeInfo:rest@_) = if authorName == redmineUserDisplay config
		then Event
			{
				project = Nothing, -- TODO
				desc = bugTitle,
				extraInfo =  bugComment,
				fullContents = Nothing,
				eventDate = localTimeToUTC timezone localTime
			} : (parseBugNodes config day timezone rest)
		else parseBugNodes config day timezone rest
	where
		bugTitle = firstNodeInnerText $ queryT [jq|a|] bugInfo
		localTime = LocalTime day (TimeOfDay hour mins 0)
		(hour, mins) = parseTimeOfDay timeOfDayStr
		timeOfDayStr = firstNodeInnerText $ queryT [jq|span.time|] bugInfo
		bugComment = firstNodeInnerText $ queryT [jq|span.description|] changeInfo
		authorName = firstNodeInnerText $ queryT [jq|span.author a|] changeInfo
		firstNodeInnerText = innerTextN . node . head
parseBugNodes _ _ _ [] = []

parseTimeOfDay :: T.Text -> (Int, Int)
parseTimeOfDay timeOfDayStr = (hours, mins)
	where
		(_hoursStr:minsStr:[]) = T.splitOn ":" (T.take 5 timeOfDayStr)
		(_hours:mins:[]) = fmap (Util.safePromise . decimal) [_hoursStr, minsStr]
		hours = if T.drop 6 timeOfDayStr == "pm"
			then _hours + 12
			else _hours

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False
