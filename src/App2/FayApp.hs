{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax #-}

import Prelude hiding ((++))
import FFI
import JQuery
import qualified Fay.Text as T
import Fay.Text (fromString, Text)

(++) = T.append

-- TODO copy-pasted from Main.Event.hs for now...
-- still problems: date and string/text
data Event = Event
	{
		pluginName :: Text,
		eventDate :: Text,
		desc :: Text,
		extraInfo :: Text, 
		fullContents :: Nullable Text -- it's maybe on the server
	}

main :: Fay ()
main = ready $ do
	setupDatepicker fetchDay
	overwriteCss
	todayServerDate >>= fetchDay

fetchDay :: Text -> Fay ()
fetchDay dayStr = do
	pleaseHold <- select "#pleasehold"
	shadow <- select "#shadow"
	unhide shadow
	unhide pleaseHold
	myajax ("/timesheet/" ++ dayStr) (processResults pleaseHold shadow)

processResults :: JQuery -> JQuery -> [Main.Event] -> Fay ()
processResults pleaseHold shadow events = do
	setSidebar ""
	table <- select "table#eventsTable tbody"
	empty table
	setScrollTop 0 table
	mapM_ (addEventRow table) events
	hide Instantly pleaseHold
	hide Instantly shadow
	return ()

addEventRow :: JQuery -> Main.Event -> Fay JQuery
addEventRow table event = do
	row <- (select $ makeEventRow event) >>= appendTo table
	click (\_ -> eventRowSelected event row) row

eventRowSelected :: Main.Event -> JQuery -> Fay ()
eventRowSelected event row = do
	--putStrLn $ "click " ++ desc event -- >>
	parent row >>= childrenMatching "tr" >>= removeClass "active"
	addClass "active" row
	setSidebar $ nullable "" id (fullContents event)
	return ()

nullable :: b -> (a->b) -> Nullable a -> b
nullable b _ Null = b
nullable _ f (Nullable x) = f x

setSidebar :: Text -> Fay JQuery
setSidebar text = select "#sidebar" >>= setScrollTop 0 >>= (setHtml $ text)

makeEventRow :: Main.Event -> Text
makeEventRow event = makeTableRow $ map ($ event)
			[formatTime . eventDate, pluginName, desc, extraInfo]

makeTableRow :: [Text] -> Text
makeTableRow cols = "<tr><td>" ++ T.intercalate "</td><td>" cols ++ "</td></tr>"

formatTime :: Text -> Text
formatTime = ffi "formatTime(%1)"

overwriteCss :: Fay ()
overwriteCss = ffi "overwriteCss()"

-- try to migrate to the ajax call from fay-jquery
-- at least don't duplicate between FayApp and FayConfig!
myajax :: Text -> (Automatic b -> Fay ()) -> Fay ()
myajax = ffi "jQuery.ajax(%1, {'type': 'GET', contentType: 'text/json', processData: false, 'success' : %2 })"

setupDatepicker :: (Text -> Fay ()) -> Fay ()
setupDatepicker = ffi "setupDatepicker(%1)"

todayServerDate :: Fay Text
todayServerDate = ffi "todayServerDate()"
