{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, OverloadedStrings, TypeFamilies #-}

module Common where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Document
import GHCJS.DOM.Types hiding (Text, Event)

import Reflex.Dom
import Data.Dependent.Sum (DSum ((:=>)))
import Reflex.Host.Class
import Data.String

import Data.Maybe
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Map (Map)
import Control.Monad.IO.Class
import Data.Aeson
import Control.Monad

data ActiveView = ActiveViewEvents | ActiveViewConfig deriving (Eq, Show)

foreign import javascript unsafe "$('#'+$1).modal('hide')" _hideModalIdDialog :: JSString -> IO ()
hideModalIdDialog :: String -> IO ()
hideModalIdDialog = _hideModalIdDialog . toJSString

foreign import javascript unsafe "$('#'+$1).modal('show')" _showModalIdDialog :: JSString -> IO ()
showModalIdDialog :: String -> IO ()
showModalIdDialog = _showModalIdDialog . toJSString

unwrapElt :: El t -> JSRef Element
unwrapElt = unElement . toElement . _el_element

eltStripClass :: IsElement self => self -> Text -> IO ()
eltStripClass elt className = do
    curClasses <- T.splitOn " " <$> T.pack <$> elementGetClassName elt
    let newClasses = T.unpack <$> filter (/= className) curClasses
    elementSetClassName elt (unwords newClasses)

attrOptDyn :: a -> String -> Bool -> String -> Map a String
attrOptDyn attr opt p s = attr =: (s <> if p then " " <> opt else "")

styleWithHideIf :: Bool -> String -> Map String String
styleWithHideIf p s = "style" =: (rest <> if p then "display: none" else "display: block")
    where rest = if null s then "" else s <> "; "

styleHideIf :: Bool -> Map String String
styleHideIf p = styleWithHideIf p ""

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank

text_ :: MonadWidget t m => Text -> m ()
text_ = text . T.unpack

performOnChange :: MonadWidget t m => (a -> WidgetHost m ()) -> Dynamic t a -> m ()
performOnChange action dynamic = performEvent_ $
    fmap (const $ sample (current dynamic) >>= action) $ updated dynamic

button' :: MonadWidget t m => String -> m (Event t ())
button' s = do
  (e, _) <- elAttr' "button" ("class" =: "btn btn-secondary btn-sm") $ text s
  return $ domEvent Click e

-- very similar to fireEventRef from Reflex.Host.Class
-- which I don't have right now.
-- #reflex-frp on freenode.net, 2015-12-25:
-- [21:38] <ryantrinkle> the only thing you might want to improve later
--         is that you could make it so that it subscribes to the event lazily
-- [21:39] <ryantrinkle> and it unsubscribes when the event gets garbage collected
-- [21:39] <ryantrinkle> https://hackage.haskell.org/package/reflex-dom-0.2/docs/src/Reflex-Dom-Widget-Basic.html#wrapDomEventMaybe
handleTrigger :: MonadIO m => ([DSum tag] -> m ()) -> a -> IORef (Maybe (tag a)) -> m ()
handleTrigger runWithActions v trigger = liftIO (readIORef trigger) >>= \case
        Nothing       -> return ()
        Just eTrigger -> runWithActions [eTrigger :=> v]

data ModalDialogResult t a = ModalDialogResult
     {
         modalElt      :: El t,
         bodyResult    :: Dynamic t a,
         okBtnEvent    :: Event t (),
         closeBtnEvent :: Event t (),
         closedEvent   :: Event t ()
     }

data ButtonInfo = PrimaryBtn String | DangerBtn String | NoBtn

setupModal :: MonadWidget t m => ModalLevel -> a -> Event t () -> m (Event t b) -> m (Event t b)
setupModal modalLevel displayData showEvent buildDialog = do
    let modalId = topLevelModalId modalLevel
    performEvent_ $ fmap (const $ liftIO $ showModalIdDialog modalId) showEvent
    modalDyn <- holdDyn Nothing $ fmap (const $ Just displayData) showEvent
    dynModalVal <- forDyn modalDyn $ fmap (const buildDialog)
    readModalResult modalLevel dynModalVal

readModalResult :: MonadWidget t m => ModalLevel -> Dynamic t (Maybe (m (Event t a))) -> m (Event t a)
readModalResult modalLevel dynModalVal = do
    dynModalEvtEvt <- dynModal modalLevel dynModalVal
    dynModalDynEvt <- holdDyn never dynModalEvtEvt
    return (switch $ current dynModalDynEvt)

readDynMonadicEvent :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
readDynMonadicEvent dynMonadicEvent = do
    eventEvt <- dyn dynMonadicEvent
    dynEvt <- holdDyn never eventEvt
    return $ switch (current dynEvt)

buildModalBody :: MonadWidget t m => String -> ButtonInfo
                 -> Dynamic t String -> m a -> m (a, Event t (), Event t ())
buildModalBody title okBtnInfo dynErrMsg contents = do
    let (okBtnText, okBtnClass, okVisible) = case okBtnInfo of
            PrimaryBtn txt -> (txt, "primary", True)
            DangerBtn txt  -> (txt, "danger", True)
            NoBtn          -> ("", "primary", False)
    elAttr "div" ("class" =: "modal-content") $ do
        elAttr "div" ("class" =: "modal-header") $ do
            void $ elAttr "button" ("type" =: "button" <> "class" =: "close"
                                    <> "data-dismiss" =: "modal"
                                    <> "aria-label" =: "Close") $
                elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
            elAttr "h4" ("class" =: "modal-title") $ text title
        bodyRes <- elAttr "div" ("class" =: "modal-body") $ do
            dynAttrs <- forDyn dynErrMsg $ \errMsg ->
                "class" =: "alert alert-danger"
                <> "role" =: "alert"
                <> styleHideIf (null errMsg)
            elDynAttr "div" dynAttrs $ do
                elAttr "strong" ("style" =: "padding-right: 7px") $ text "Error"
                dynText dynErrMsg
            contents
        (okEvt, closeEvt)  <- elAttr "div" ("class" =: "modal-footer") $ do
            (closeEl, _) <- elAttr' "button" ("type" =: "button"
                                              <> "class" =: "btn btn-secondary"
                                              <> "data-dismiss" =: "modal")
                $ text "Close"
            okEl <- addOkButton okBtnClass okBtnText okVisible
            return (domEvent Click okEl, domEvent Click closeEl)
        return (bodyRes, okEvt, closeEvt)

addOkButton :: MonadWidget t m => String -> String -> Bool -> m (El t)
addOkButton okBtnClass okBtnText visible = do
    let style = "style" =: if visible then "" else "display: none"
    (okEl, _) <- elAttr' "button" ("type" =: "button"
                                   <> "class" =: ("btn btn-" <> okBtnClass)
                                   <> style)
        $ text okBtnText
    return okEl

-- a secondary modal is on top of the basic one (modal in modal)
data ModalLevel = ModalLevelBasic | ModalLevelSecondary

topLevelModalId :: ModalLevel -> String
topLevelModalId ModalLevelBasic = "toplevelmodal"
topLevelModalId ModalLevelSecondary = "toplevelsecmodal"

topLevelModalContentsId :: ModalLevel -> String
topLevelModalContentsId ModalLevelBasic = "toplevelmodalcontents"
topLevelModalContentsId ModalLevelSecondary = "toplevelsecmodalcontents"

dynModal :: MonadWidget t m => ModalLevel -> Dynamic t (Maybe (m a)) -> m (Event t a)
dynModal modalLevel = dynAtEltId (topLevelModalContentsId modalLevel)

-- | this is copy-pasted & modified from 'dyn' from reflex-dom
-- instead of appending the nodes at the current position in the
-- DOM, append them under the node by the ID which you give.
dynAtEltId :: MonadWidget t m => String -> Dynamic t (Maybe (m a)) -> m (Event t a)
dynAtEltId eltId child = do
    (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
    let e = fmap snd newChildBuilt
    childVoidAction <- hold never e
    performEvent_ $ fmap (const $ return ()) e
    addVoidAction $ switch childVoidAction
    doc <- askDocument
    runW <- getRunWidget
    let build = \case
            Nothing -> return ()
            Just c  -> do
                Just df <- liftIO $ documentCreateDocumentFragment doc
                (result, postBuild, voidActions) <- runW df c
                runFrameWithTriggerRef newChildBuiltTriggerRef (result, voidActions)
                postBuild
                Just root <- liftIO $ documentGetElementById doc eltId
                lastChild <- liftIO $ nodeGetLastChild root
                void $ liftIO $ nodeReplaceChild root (Just df) lastChild
    schedulePostBuild $ do
        c <- sample $ current child
        build c
    addVoidAction $ ffor (updated child) $ \newChild -> do
        build newChild
    return $ fmap fst newChildBuilt

rawPointerSpan :: MonadWidget t m => Dynamic t String -> m ()
rawPointerSpan = rawSpan ("style" =: "cursor: pointer")

rawSpan :: MonadWidget t m => Map String String -> Dynamic t String -> m ()
rawSpan attrs = void . elDynHtmlAttr' "span" attrs

getGlyphiconUrl :: String -> String
getGlyphiconUrl base = "glyphicons_free/glyphicons/png/" <> base <> ".png"

data RemoteData a = RemoteDataInvalid String | RemoteDataLoading | RemoteData a deriving Show

instance Functor RemoteData where
    fmap _ RemoteDataLoading = RemoteDataLoading
    fmap _ (RemoteDataInvalid x) = RemoteDataInvalid x
    fmap f (RemoteData a) = RemoteData (f a)

instance Applicative RemoteData where
    pure = RemoteData
    RemoteData f <*> r = fmap f r
    (RemoteDataInvalid x) <*> _ = RemoteDataInvalid x
    RemoteDataLoading <*> _ = RemoteDataLoading

instance Monad RemoteData where
    (RemoteDataInvalid x) >>= _ = RemoteDataInvalid x
    RemoteDataLoading >>= _ = RemoteDataLoading
    RemoteData x >>= f = f x

readEmptyRemoteData :: XhrResponse -> RemoteData ()
readEmptyRemoteData XhrResponse{..} = case _xhrResponse_status of
    200 -> case _xhrResponse_body of
        Nothing -> RemoteData ()
        Just "" -> RemoteData ()
        Just x -> RemoteDataInvalid $ "Expected empty response, got" <> T.unpack x
    _ -> RemoteDataInvalid $ "HTTP response code " <> show _xhrResponse_status
             <> "; details: " <> T.unpack (fromMaybeEmpty "none" _xhrResponse_body)

readRemoteData :: FromJSON a => XhrResponse -> RemoteData a
readRemoteData XhrResponse{..} = case _xhrResponse_status of
    200 -> case _xhrResponse_body of
        Nothing -> RemoteDataInvalid "Empty server response"
        Just rawData -> case decodeText rawData of
            Nothing -> RemoteDataInvalid $
                "JSON has invalid format: " <> T.unpack (fromMaybe "Nothing" _xhrResponse_body)
            Just decoded -> RemoteData decoded
    _ -> RemoteDataInvalid $ "HTTP response code " <> show _xhrResponse_status
             <> "; details: " <> T.unpack (fromMaybeEmpty "none" _xhrResponse_body)

fromMaybeEmpty :: (IsString a, Eq a) => a -> Maybe a -> a
fromMaybeEmpty val Nothing = val
fromMaybeEmpty val (Just "") = val
fromMaybeEmpty _ (Just r) = r

isRemoteDataLoading :: RemoteData a -> Bool
isRemoteDataLoading RemoteDataLoading = True
isRemoteDataLoading _ = False

remoteDataInvalidDesc :: RemoteData a -> Maybe String
remoteDataInvalidDesc (RemoteDataInvalid x) = Just x
remoteDataInvalidDesc _ = Nothing

remoteDataErrorDescDyn :: MonadWidget t m => Event t (RemoteData a) -> m (Dynamic t String)
remoteDataErrorDescDyn evt = holdDyn "" (fmapMaybe remoteDataInvalidDesc evt)

fromRemoteData :: RemoteData a -> Maybe a
fromRemoteData (RemoteData x) = Just x
fromRemoteData _ = Nothing

makeSimpleXhr :: (MonadWidget t m, FromJSON a) => String -> Event t b -> m (Dynamic t (RemoteData a))
makeSimpleXhr url = makeSimpleXhr' (const url)

makeSimpleXhr' :: (MonadWidget t m, FromJSON a) => (b -> String) -> Event t b -> m (Dynamic t (RemoteData a))
makeSimpleXhr' getUrl evt = do
    req <- performRequestAsync $ (\evtVal -> xhrRequest "GET" (getUrl evtVal) def) <$> evt
    holdDyn RemoteDataLoading $ fmap readRemoteData req
