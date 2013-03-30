{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.ByteString as BS (ByteString(..), concat)
import Data.ByteString.Char8 as Char8 (split, pack)
import Data.ByteString.Lazy (fromChunks)
import System.IO.Streams (write)
import Network.Http.Client
import Data.Maybe
import Data.Text as T (Text(..), splitOn, pack)
import Data.Text.Encoding as TE
import Text.Printf
import Data.Time.Calendar
import Data.List (find)

import Text.XML (Node(..))
import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Scraping
import Text.HTML.DOM (parseLBS)

import qualified Util

import Debug.Trace

redmineUrl = "http://redmine/"
redmineUsername = "emmanuel.touzery@lecip-its.com"
redmineUserDisplay = "Emmanuel Touzery"
redminePassword = "itak2030"

data BugActionInfo = BugActionInfo
	{
		comment :: T.Text
	} deriving Show

main = do
	let day = fromGregorian 2013 3 28
	maybeCookie <- login redmineUsername redminePassword
	let cookieRows = split '\n' $ fromJust maybeCookie
	let cookieValues = fmap (head . (split ';')) cookieRows
	let activityUrl = prepareActivityUrl day
	response <- Util.http activityUrl "" concatHandler $ do
		http GET "/activity"
		setHeader "Cookie" (cookieValues !! 1)
	print $ getIssues response day

prepareActivityUrl :: Day -> ByteString
prepareActivityUrl day = BS.concat ["http://redmine/activity?from=", dayBeforeStr]
	where
		dayBefore = addDays (-1) day
		(y, m, d) = toGregorian dayBefore
		dayBeforeStr = Char8.pack $ printf "%d-%02d-%02d" y m d

-- returns the cookie
login :: ByteString -> ByteString -> IO (Maybe ByteString)
login username password = do
	postForm
		(BS.concat [redmineUrl, "login"])
		[("username", redmineUsername), ("password", redminePassword)]
		(\r i -> return $ getHeader r "Set-Cookie")

-- will return Nothing if the date you want is not covered in the
-- page.
-- TODO: that may mean we need to do paging, or simply that there
-- was no activity on that day.. For instance it was week-end..
getIssues :: ByteString -> Day -> Maybe [BugActionInfo]
getIssues html day = fmap getIssuesForDayNode dayNode
	where
		doc = fromDocument $ parseLBS $ fromChunks [html]
		dayNodes = queryT [jq| div#content div#activity h3 |] doc
		dayNode = find (isDayTitle day) dayNodes

isDayTitle :: Day -> Cursor -> Bool
isDayTitle day nod = dayTitle == innerTextN (node nod)
	where
		(y, m, d) = toGregorian day
		dayTitle = T.pack $ printf "%02d/%02d/%4d" m d y

getIssuesForDayNode :: Cursor -> [BugActionInfo]
getIssuesForDayNode dayNode = parseBugNodes bugNodes
	where
		bugNodes = filter (isElement . node) (child dlNode)
		(Just dlNode) = find (isElement . node) (following dayNode)

parseBugNodes :: [Cursor] -> [BugActionInfo]
parseBugNodes (bugInfo:changeInfo:rest@_) = if authorName == redmineUserDisplay
		then BugActionInfo { comment =  bugComment } : (parseBugNodes rest)
		else parseBugNodes rest
	where
		bugComment = firstNodeInnerText $ queryT [jq|span.description|] changeInfo
		authorName = firstNodeInnerText $ queryT [jq|span.author a|] changeInfo
		firstNodeInnerText = innerTextN . node . head
parseBugNodes [] = []

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False
