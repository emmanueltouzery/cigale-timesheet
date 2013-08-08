{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
import FFI
import JQuery

-- TODO copy-pasted from Main.Event.hs for now...
-- still problems: date and string/text
data Event = Event
	{
		pluginName :: String,
		eventDate :: String,
		desc :: String,
		extraInfo :: String, 
		fullContents :: Nullable String -- it's maybe on the server
	}

main :: Fay ()
main = ready $ do
	setupDatepicker fetchDay
	overwriteCss
	todayServerDate >>= fetchDay

fetchDay :: String -> Fay ()
fetchDay dayStr = do
	pleaseHold <- select "#pleasehold"
	unhide pleaseHold
	myajax ("/timesheet/" ++ dayStr) (processResults pleaseHold)

processResults :: JQuery -> [Main.Event] -> Fay ()
processResults pleaseHold events = do
	setSidebar ""
	table <- select "table#eventsTable tbody"
	empty table
	mapM_ (addEventRow table) events
	hide Instantly pleaseHold
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

setSidebar :: String -> Fay JQuery
setSidebar text = select "#sidebar" >>= setScrollTop 0 >>= setHtml text

makeEventRow :: Main.Event -> String
makeEventRow event = makeTableRow $ map ($ event)
			[formatTime . eventDate, pluginName, desc, extraInfo]

makeTableRow :: [String] -> String
makeTableRow cols = "<tr><td>" ++ intercalate "</td><td>" cols ++ "</td></tr>"

formatTime :: String -> String
formatTime = ffi "formatTime(%1)"

overwriteCss :: Fay ()
overwriteCss = ffi "overwriteCss()"

-- try to migrate to the ajax call from fay-jquery
-- at least don't duplicate between FayApp and FayConfig!
myajax :: String -> (Automatic b -> Fay ()) -> Fay ()
myajax = ffi "jQuery.ajax(%1, {'type': 'GET', contentType: 'text/json', processData: false, 'success' : %2 })"

setupDatepicker :: (String -> Fay ()) -> Fay ()
setupDatepicker = ffi "setupDatepicker(%1)"

todayServerDate :: Fay String
todayServerDate = ffi "todayServerDate()"
