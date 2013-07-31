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
		fullContents :: String -- removed the Maybe
	}

main :: Fay ()
main = --ready $ do
		myajax "/timesheet/2013-07-01" processResults

processResults :: [Main.Event] -> Fay ()
processResults events = do
	table <- select "table#eventsTable"
	mapM_ (addEventRow table) events
	select "#pleasehold" >>= hide Instantly
	return ()

addEventRow :: JQuery -> Main.Event -> Fay JQuery
addEventRow table event = do
	row <- (select $ makeEventRow event) >>= appendTo table
	click (\_ -> eventRowSelected event row) row

eventRowSelected :: Main.Event -> JQuery -> Fay ()
eventRowSelected event row = do
	--putStrLn $ "click " ++ desc event -- >>
	parent row >>= childrenMatching "tr" >>= removeClass "current"
	addClass "current" row
	return ()

makeEventRow :: Main.Event -> String
makeEventRow event = makeTableRow $ map ($ event)
			[formatTime . eventDate, pluginName, desc, extraInfo]

makeTableRow :: [String] -> String
makeTableRow cols = "<tr><td>" ++ intercalate "</td><td>" cols ++ "</td></tr>"

formatTime :: String -> String
formatTime = ffi "formatTime(%1)"

myajax :: String -> (Automatic b -> Fay ()) -> Fay ()
myajax = ffi "jQuery.ajax(%1, {'type': 'GET', contentType: 'text/json', processData: false, 'success' : %2 })"
