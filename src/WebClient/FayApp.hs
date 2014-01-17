{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax, EmptyDataDecls #-}

import Prelude hiding ((++))
import FFI
import JQuery hiding (Event, not)
import qualified Fay.Text as T
import Fay.Text (fromString, Text)
import Knockout
import Utils

(++) = T.append

data JDate

data Event = Event
	{
		pluginName :: Text,
		eventIcon :: Text,
		eventDate :: Text,
		desc :: Text,
		extraInfo :: Text, 
		fullContents :: Nullable Text -- it's maybe on the server
	} | NullEvent

-- I would prever to implement Eq for Event,
-- but it didn't work.
x === y = (eventDate x) == (eventDate y) &&
		(eventIcon x) == (eventIcon y) &&
		(pluginName x) == (pluginName y) &&
		(desc x) == (desc y) &&
		(extraInfo x) == (extraInfo y)

data MainViewModel = MainViewModel
	{
		displayedDate :: Observable JDate,
		eventsObs :: ObservableArray Event,
		selectedEvent :: Observable Event,
		showSidebar :: MainViewModel -> Event -> Fay (),
		isActive :: MainViewModel -> Event -> Fay Bool
	}
instance KnockoutModel MainViewModel

main :: Fay ()
main = ready $ do
	eventsObsV <- ko_observableList []
	jsDate <- getJsDate "1970-01-01"
	let viewModel = MainViewModel
		{
			displayedDate = ko_observable jsDate,
			eventsObs = eventsObsV,
			showSidebar = showSidebarCb,
			selectedEvent = ko_observable NullEvent,
			isActive = \vm event -> liftM (=== event) (ko_get $ selectedEvent vm)
		}
	
	ko_applyBindings viewModel
	setupDatepicker (fetchDay viewModel)
	overwriteCss
	todayServerDate >>= (fetchDay viewModel)

fetchDay :: MainViewModel -> Text -> Fay ()
fetchDay viewModel dayStr = do
	jdate <- getJsDate dayStr
	ko_set (displayedDate viewModel) jdate
	pleaseHold <- select "#pleasehold"
	shadow <- select "#shadow"
	unhide shadow
	unhide pleaseHold
	myajax ("/timesheet/" ++ dayStr) (processResults viewModel pleaseHold shadow)
		(handleError pleaseHold shadow)

getJsDate :: Text -> Fay JDate
getJsDate = ffi "new Date(Date.parse(%1 + 'T00:00:00'))"

handleError :: JQuery -> JQuery -> JqXHR -> Text -> Text -> Fay ()
handleError pleaseHold shadow jqXhr textStatus errorThrown = do
	hide Instantly pleaseHold
	hide Instantly shadow
	alert $ T.concat ["Error! ", responseText jqXhr]
	return ()

processResults :: MainViewModel -> JQuery -> JQuery -> [Main.Event] -> Fay ()
processResults viewModel pleaseHold shadow events = do
	table <- select "table#eventsTable tbody"
	empty table
	setScrollTop 0 table
	let eventsObsV =  eventsObs viewModel
	ko_removeAllObservableArray eventsObsV
	mapM_ (\e -> ko_pushObservableArray eventsObsV (evtFormatTime e)) events
	when (not $ null events) $ showSidebarCb viewModel (evtFormatTime $ head events)
	hide Instantly pleaseHold
	hide Instantly shadow
	return ()

showSidebarCb :: MainViewModel -> Event -> Fay ()
showSidebarCb vm event = do
	ko_set (selectedEvent vm) event
	select "#sidebar" >>= setScrollTop 0
	return ()

nullable :: b -> (a->b) -> Nullable a -> b
nullable b _ Null = b
nullable _ f (Nullable x) = f x

evtFormatTime :: Main.Event -> Main.Event
evtFormatTime event = event { eventDate = formatTime $ eventDate event }

formatTime :: Text -> Text
formatTime = ffi "formatTime(%1)"

overwriteCss :: Fay ()
overwriteCss = ffi "overwriteCss()"

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

data JqXHR

responseText :: JqXHR -> Text
responseText = ffi "%1.responseText"

-- try to migrate to the ajax call from fay-jquery
-- at least don't duplicate between FayApp and FayConfig!
myajax :: Text -> (Automatic b -> Fay ()) -> (JqXHR -> Text -> Text -> Fay ()) -> Fay ()
myajax = ffi "jQuery.ajax(%1, {'type': 'GET', contentType: 'text/json', processData: false, 'success' : %2, 'error': %3 })"

setupDatepicker :: (Text -> Fay ()) -> Fay ()
setupDatepicker = ffi "setupDatepicker(%1)"

todayServerDate :: Fay Text
todayServerDate = ffi "todayServerDate()"
