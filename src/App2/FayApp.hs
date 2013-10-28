{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax, EmptyDataDecls #-}

import Prelude hiding ((++))
import FFI
import JQuery hiding (Event)
import qualified Fay.Text as T
import Fay.Text (fromString, Text)
import Knockout
import Utils

(++) = T.append

-- TODO copy-pasted from Main.Event.hs for now...
-- I would prefer not to have the NullEvent constructor
-- and instead have "Nullable Event" in the viewmodel,
-- but it didn't work.
data Event = Event
	{
		pluginName :: Text,
		eventDate :: Text,
		desc :: Text,
		extraInfo :: Text, 
		fullContents :: Nullable Text -- it's maybe on the server
	} | NullEvent

-- I would prever to implement Eq for Event,
-- but it didn't work.
x === y = (eventDate x) == (eventDate y) &&
		(pluginName x) == (pluginName y) &&
		(desc x) == (desc y) &&
		(extraInfo x) == (extraInfo y)

data MainViewModel = MainViewModel
	{
		eventsObs :: ObservableArray Event,
		selectedEvent :: Observable Event,
		showSidebar :: MainViewModel -> Event -> Fay (),
		isActive :: MainViewModel -> Event -> Fay Bool
	}
instance KnockoutModel MainViewModel

main :: Fay ()
main = ready $ do
	eventsObsV <- ko_observableList []
	let viewModel = MainViewModel
		{
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
	pleaseHold <- select "#pleasehold"
	shadow <- select "#shadow"
	unhide shadow
	unhide pleaseHold
	myajax ("/timesheet/" ++ dayStr) (processResults viewModel pleaseHold shadow)

processResults :: MainViewModel -> JQuery -> JQuery -> [Main.Event] -> Fay ()
processResults viewModel pleaseHold shadow events = do
	setSidebar ""
	table <- select "table#eventsTable tbody"
	empty table
	setScrollTop 0 table
	let eventsObsV =  eventsObs viewModel
	ko_removeAllObservableArray eventsObsV
	mapM_ (\e -> ko_pushObservableArray eventsObsV (evtFormatTime e)) events
	hide Instantly pleaseHold
	hide Instantly shadow
	return ()

showSidebarCb :: MainViewModel -> Event -> Fay ()
showSidebarCb vm event = do
	ko_set (selectedEvent vm) event
	setSidebar $ nullable "" id (fullContents event)
	return ()

nullable :: b -> (a->b) -> Nullable a -> b
nullable b _ Null = b
nullable _ f (Nullable x) = f x

setSidebar :: Text -> Fay JQuery
setSidebar text = select "#sidebar" >>= setScrollTop 0 >>= (setHtml $ text)

evtFormatTime :: Main.Event -> Main.Event
evtFormatTime event = event { eventDate = formatTime $ eventDate event}

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
