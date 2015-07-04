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

data FetchResponse = FetchResponse
    {
        fetchedEvents :: [Event],
        fetchErrors :: [Text]
    }

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
x === y = eventDate x == eventDate y &&
        eventIcon x == eventIcon y &&
        pluginName x == pluginName y &&
        desc x == desc y &&
        extraInfo x == extraInfo y

data MainViewModel = MainViewModel
    {
        displayedDate :: Observable JDate,
        eventsObs :: ObservableList Event,
        selectedEvent :: Observable Event,
        showSidebar :: MainViewModel -> Event -> Fay (),
        isActive :: MainViewModel -> Event -> Fay Bool,
        warningText :: Observable Text
    }
instance KnockoutModel MainViewModel

main :: Fay ()
main = ready $ do
    eventsObsV <- koObservableList []
    jsDate <- getJsDate "1970-01-01"
    let viewModel = MainViewModel {
            displayedDate = koObservable jsDate,
            eventsObs = eventsObsV,
            showSidebar = showSidebarCb,
            selectedEvent = koObservable NullEvent,
            isActive = \vm event -> liftM (=== event) (koGet $ selectedEvent vm),
            warningText = koObservable ""
        }
    koApplyBindings viewModel
    setupDatepicker (fetchDay viewModel)
    overwriteCss
    yesterdayServerDate >>= fetchDay viewModel

fetchDay :: MainViewModel -> Text -> Fay ()
fetchDay viewModel dayStr = do
    jdate <- getJsDate dayStr
    koSet (displayedDate viewModel) jdate
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

processResults :: MainViewModel -> JQuery -> JQuery -> FetchResponse -> Fay ()
processResults viewModel pleaseHold shadow fetchResponse = do
    T.intercalate ", " (fetchErrors fetchResponse) ~> warningText viewModel
    let events = fetchedEvents fetchResponse
    table <- select "table#eventsTable tbody"
    empty table
    setScrollTop 0 table
    let eventsObsV =  eventsObs viewModel
    koRemoveAllObservableList eventsObsV
    mapM_ (koPushObservableList eventsObsV . evtFormatTime) events
    let eventToDisplay = if null events then NullEvent else evtFormatTime $ head events
    showSidebarCb viewModel eventToDisplay
    -- recalculate the heights in case the warning
    -- box appeared or disappeared
    overwriteCss
    updateSidebarContentsHeight

    hide Instantly pleaseHold
    void $ hide Instantly shadow

showSidebarCb :: MainViewModel -> Event -> Fay ()
showSidebarCb vm event = do
    event ~> selectedEvent vm
    void $ select "#sidebar" >>= setScrollTop 0
    updateSidebarContentsHeight

nullable :: b -> (a->b) -> Nullable a -> b
nullable b _ Null = b
nullable _ f (Nullable x) = f x

evtFormatTime :: Main.Event -> Main.Event
evtFormatTime event = event { eventDate = formatTime $ eventDate event }

formatTime :: Text -> Text
formatTime = ffi "formatTime(%1)"

overwriteCss :: Fay ()
overwriteCss = ffi "overwriteCss()"

updateSidebarContentsHeight :: Fay ()
updateSidebarContentsHeight = ffi "updateSidebarContentsHeight()"

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

yesterdayServerDate :: Fay Text
yesterdayServerDate = ffi "yesterdayServerDate()"
