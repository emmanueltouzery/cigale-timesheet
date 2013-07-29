{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
import FFI

-- TODO copy-pasted from Event.hs for now...
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
		ajax "/timesheet/2013-07-01" processResults

processResults :: [Event] -> Fay ()
processResults events = do
		mapM_ processResult events
		nodes "#pleasehold" >>= hide
		return ()
	where
		eventsTable = nodes "table#eventsTable"
		processResult event = eventsTable >>= (append $ makeEventRow event)

makeEventRow :: Event -> String
makeEventRow event = makeTableRow $ map ($ event) [\x -> formatTime $ eventDate x,
							pluginName, desc, extraInfo]

makeTableRow :: [String] -> String
makeTableRow cols = "<tr><td>" ++ intercalate "</td><td>" cols ++ "</td></tr>"

data JQuery

formatTime :: String -> String
formatTime = ffi "formatTime(%1)"

ajax :: String -> (Automatic b -> Fay ()) -> Fay ()
ajax = ffi "jQuery.ajax(%1, {'type': 'GET', contentType: 'text/json', processData: false, 'success' : %2 })"

nodes :: String -> Fay JQuery
nodes = ffi "jQuery(%1)"

append :: String -> JQuery -> Fay JQuery
append = ffi "%2.append(%1)"

hide :: JQuery -> Fay JQuery
hide = ffi "%1.hide()"
