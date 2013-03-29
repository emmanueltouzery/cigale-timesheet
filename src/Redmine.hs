{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.ByteString as BS (ByteString(..), concat)
import Data.ByteString.Char8 (split)
import Data.ByteString.Lazy (fromChunks)
import System.IO.Streams
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

import Debug.Trace

redmineUrl = "http://redmine/"
redmineUsername = "emmanuel.touzery@lecip-its.com"
redminePassword = "itak2030"

data BugBasicInfo = BugBasicInfo
	{
		bugBasicId :: Int
--		bugBasicUpdated :: LocalTime
	} deriving Show

-- which HTTP api did i use? check ical...
-- otherwise this one is available:
--http://blogs.operationaldynamics.com/andrew/software/haskell/http-streams-introduction

main = do
	let day = fromGregorian 2013 3 28
	maybeCookie <- login redmineUsername redminePassword
	let cookieRows = split '\n' $ fromJust maybeCookie
	let cookieValues = fmap (head . (split ';')) cookieRows
	response <- withConnection (openConnection "redmine" 80) (getActivity cookieValues)
	print $ getIssues response day

getActivity :: [ByteString] -> Connection -> IO ByteString
getActivity cookieValues c = do
	q <- buildRequest $ do
		http GET "/activity"
		setHeader "Cookie" (cookieValues !! 1)
	sendRequest c q emptyBody
	receiveResponse c concatHandler

-- returns the cookie
login :: ByteString -> ByteString -> IO (Maybe ByteString)
login username password = do
	postForm
		(BS.concat [redmineUrl, "login"])
		[("username", redmineUsername), ("password", redminePassword)]
		getCookies

getCookies :: Response -> InputStream ByteString -> IO (Maybe ByteString)
getCookies r i = return $ getHeader r "Set-Cookie"

-- will return Nothing if the date you want is not covered in the
-- page.
-- TODO: that may mean we need to do paging, or simply that there
-- was no activity on that day.. For instance it was week-end..
getIssues :: ByteString -> Day -> Maybe [BugBasicInfo]
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

getIssuesForDayNode :: Cursor -> [BugBasicInfo]
getIssuesForDayNode dayNode = traceShow (dlNode) []
	where dlNode = find (isElement . node) (following dayNode)

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False
