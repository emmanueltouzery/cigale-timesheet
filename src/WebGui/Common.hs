{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, OverloadedStrings, TypeFamilies #-}

module Common where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Document
import GHCJS.DOM.Types hiding (Text, Event)

import Reflex.Dom hiding (display)
import Data.Dependent.Sum (DSum ((:=>)))
import Reflex.Host.Class
import Data.String

import Clay as C hiding (filter, title, contents, action, url)
import Data.Maybe
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
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
attrOptDyn attrib opt isShow str = attrib =: (str <> if isShow then " " <> opt else "")

attrStyleWithHideIf :: Bool -> Css -> Map String String
attrStyleWithHideIf isHide rest = "style" =: styleStr (styleWithHideIf isHide rest)

styleWithHideIf :: Bool -> Css -> Css
styleWithHideIf isHide rest = (rest >> when isHide (display none))

attrStyleHideIf :: Bool -> Map String String
attrStyleHideIf isHide = "style" =: styleStr (styleHideIf isHide)

styleHideIf :: Bool -> Css
styleHideIf isHide = styleWithHideIf isHide (return ())

stylesheet :: MonadWidget t m => String -> m ()
stylesheet str = elAttr "link" ("rel" =: "stylesheet" <> "href" =: str) blank

text_ :: MonadWidget t m => Text -> m ()
text_ = text . T.unpack

elStyle' :: MonadWidget t m => String -> Css -> m a -> m (El t, a)
elStyle' elementTag styleCss child = elAttrStyle' elementTag Map.empty styleCss child

elAttrStyle' :: MonadWidget t m => String -> Map String String -> Css -> m a -> m (El t, a)
elAttrStyle' elementTag attrs styleCss child =
    elAttr' elementTag (attrs <> "style" =: styleStr styleCss) child

elStyle :: MonadWidget t m => String -> Css -> m a -> m a
elStyle elementTag styleCss child = elAttrStyle elementTag Map.empty styleCss child

elAttrStyle :: MonadWidget t m => String -> Map String String -> Css -> m a -> m a
elAttrStyle elementTag attrs styleCss child =
    elAttr elementTag (attrs <> "style" =: styleStr styleCss) child

elDynAttrStyle' :: MonadWidget t m => String -> Dynamic t (Map String String) -> Css -> m a
                -> m (El t, a)
elDynAttrStyle' elementTag dynAttrs styleCss child = do
    newAttrsDyn <- forDyn dynAttrs (<> "style" =: styleStr styleCss)
    elDynAttr' elementTag newAttrsDyn child

-- the tail . init removes leading and trailing {}
-- see https://github.com/sebastiaanvisser/clay/issues/120
styleStr :: Css -> String
styleStr css = case T.unpack $ TL.toStrict $ renderWith compact [] css of
    xs@(_:_:_) -> tail $ init xs
    short      -> short

paddingAll :: Size a -> Css
paddingAll x = padding x x x x

marginAll :: Size a -> Css
marginAll x = margin x x x x

performOnDynChange :: MonadWidget t m => Dynamic t a -> (a -> WidgetHost m ()) -> m ()
performOnDynChange dynamic action = performEvent_ $
    fmap (const $ sample (current dynamic) >>= action) $ updated dynamic

button' :: MonadWidget t m => m a -> m (Event t ())
button' contents = do
    -- forcing white background, otherwise it stays to gray after
    -- being pressed which I find ugly.
    (e, _) <- elAttrStyle' "button"
        ("class" =: "btn btn-secondary btn-sm") (backgroundColor white) contents
    return $ domEvent Click e

smallIconButton :: MonadWidget t m => String -> m (Event t ())
smallIconButton = iconButton 12

iconButton :: MonadWidget t m => Int -> String -> m (Event t ())
iconButton iconHeight iconName = button' $
    elAttrStyle "img" ("src" =: getGlyphiconUrl iconName)
        (height $ px $ fromIntegral iconHeight) $ return ()

col :: MonadWidget t m => m a -> m a
col = elStyle "td" (paddingAll (px 5))

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

data ButtonInfo = PrimaryBtn String | DangerBtn String | NoBtn

setupModal :: MonadWidget t m => ModalLevel -> Event t a -> m (Event t b) -> m (Event t b)
setupModal modalLevel showEvent buildDialog = do
    showModalOnEvent modalLevel showEvent
    modalDyn <- holdDyn Nothing $ fmap Just showEvent
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
    elAttr "div" ("class" =: "modal-content") $ do
        elAttr "div" ("class" =: "modal-header") $ do
            let crossBtnAttrs = "type" =: "button" <> "class" =: "close"
                    <> "data-dismiss" =: "modal" <> "aria-label" =: "Close"
            void $ elAttr "button" crossBtnAttrs $
                elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
            elAttr "h4" ("class" =: "modal-title") $ text title
        bodyRes <- elAttr "div" ("class" =: "modal-body") $ do
            addErrorBox dynErrMsg
            contents
        (okEvt, closeEvt)  <- addModalFooter okBtnInfo
        return (bodyRes, okEvt, closeEvt)

addErrorBox :: MonadWidget t m => Dynamic t String -> m ()
addErrorBox dynErrMsg = do
    dynAttrs <- forDyn dynErrMsg $ \errMsg ->
        "class" =: "alert alert-danger"
        <> "role" =: "alert"
        <> attrStyleHideIf (null errMsg)
    elDynAttr "div" dynAttrs $ do
        elStyle "strong" (paddingRight $ px 7) $ text "Error"
        dynText dynErrMsg

addModalFooter :: MonadWidget t m => ButtonInfo -> m (Event t (), Event t ())
addModalFooter okBtnInfo = do
    let (okBtnText, okBtnClass, okVisible) = case okBtnInfo of
            PrimaryBtn txt -> (txt, "primary", True)
            DangerBtn txt  -> (txt, "danger", True)
            NoBtn          -> ("", "primary", False)
    elAttr "div" ("class" =: "modal-footer") $ do
            let closeBtnAttrs = "type" =: "button" <> "data-dismiss" =: "modal"
                    <> "class" =: "btn btn-secondary"
            (closeEl, _) <- elAttr' "button" closeBtnAttrs
                $ text "Close"
            okEl <- addOkButton okBtnClass okBtnText okVisible
            return (domEvent Click okEl, domEvent Click closeEl)

addOkButton :: MonadWidget t m => String -> String -> Bool -> m (El t)
addOkButton okBtnClass okBtnText isShow = do
    let styl = "style" =: if isShow then "" else "display: none"
    (okEl, _) <- elAttr' "button" ("type" =: "button"
                                   <> "class" =: ("btn btn-" <> okBtnClass)
                                   <> styl)
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
                Just docRoot <- liftIO $ documentGetElementById doc eltId
                nodeLastChild <- liftIO $ nodeGetLastChild docRoot
                void $ liftIO $ nodeReplaceChild docRoot (Just df) nodeLastChild
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
getGlyphiconUrl iconBase = "glyphicons_free/glyphicons/png/" <> iconBase <> ".png"

progressDialog :: MonadWidget t m => Dynamic t Int -> m ()
progressDialog percentDyn = do
    attrsDyn <- forDyn percentDyn $ \percent ->
        ("class" =: "progress" <> "value" =: show percent <> "max" =: "100")
    elDynAttr "progress" attrsDyn $ dynText =<< mapDyn ((++ "%") . show) percentDyn

showModalOnEvent :: MonadWidget t m => ModalLevel -> Event t a -> m ()
showModalOnEvent modalLevel evt = performEvent_ $ fmap
    (const $ liftIO $ showModalIdDialog $ topLevelModalId modalLevel) evt

hideModalOnEvent :: MonadWidget t m => ModalLevel -> Event t a -> m ()
hideModalOnEvent modalLevel evt = performEvent_ $ fmap
    (const $ liftIO $ hideModalIdDialog $ topLevelModalId modalLevel) evt

data RemoteData a = RemoteDataInvalid String | RemoteDataLoading | RemoteData a deriving Show

instance Functor RemoteData where
    fmap _ RemoteDataLoading = RemoteDataLoading
    fmap _ (RemoteDataInvalid x) = RemoteDataInvalid x
    fmap f (RemoteData x) = RemoteData (f x)

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
