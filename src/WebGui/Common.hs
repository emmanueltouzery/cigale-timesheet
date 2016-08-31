{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, JavaScriptFFI, FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, OverloadedStrings, TypeFamilies #-}

module Common where

import GHCJS.Types
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Document
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.Marshal

import Reflex.Dom hiding (display)
import Reflex.Host.Class
import Data.String

import Clay as C hiding (filter, title, contents, action, url)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Data.Aeson
import Control.Monad
import Data.List

data ActiveView = ActiveViewEvents | ActiveViewConfig deriving (Eq, Show)

foreign import javascript unsafe "$('#'+$1).modal('hide')" _hideModalIdDialog :: JSVal -> IO ()
hideModalIdDialog :: Text -> IO ()
hideModalIdDialog = _hideModalIdDialog <=< toJSVal

foreign import javascript unsafe "$('#'+$1).modal('show')" _showModalIdDialog :: JSVal -> IO ()
showModalIdDialog :: Text -> IO ()
showModalIdDialog = _showModalIdDialog <=< toJSVal

unwrapElt :: El t -> JSVal
unwrapElt = unElement . toElement . _el_element

eltStripClass :: IsElement self => self -> Text -> IO ()
eltStripClass elt className = do
    curClasses <- T.splitOn " " <$> T.pack <$> getClassName elt
    let newClasses = T.unpack <$> filter (/= className) curClasses
    setClassName elt (unwords newClasses)

eltToggleClass :: IsElement self => self -> Text -> IO ()
eltToggleClass elt classItem = do
    let classItemS = T.unpack classItem
    fullClass <- getClassName elt
    if classItemS `isInfixOf` fullClass
        then eltStripClass elt classItem
        else setClassName elt (fullClass <> " " <> classItemS)

attrOptDyn :: a -> String -> Bool -> String -> Map a String
attrOptDyn attrib opt isShow str = attrib =: (str <> if isShow then " " <> opt else "")

attrStyleWithHideIf :: Bool -> Css -> Map Text Text
attrStyleWithHideIf isHide rest = "style" =: styleStr (styleWithHideIf isHide rest)

styleWithHideIf :: Bool -> Css -> Css
styleWithHideIf isHide rest = rest >> when isHide (display none)

attrStyleHideIf :: Bool -> Map Text Text
attrStyleHideIf isHide = "style" =: styleStr (styleHideIf isHide)

styleHideIf :: Bool -> Css
styleHideIf isHide = styleWithHideIf isHide (return ())

stylesheet :: MonadWidget t m => Text -> m ()
stylesheet str = elAttr "link" ("rel" =: "stylesheet" <> "href" =: str) blank

elStyle' :: MonadWidget t m => Text -> Css -> m a -> m (El t, a)
elStyle' elementTag styleCss child = elAttrStyle' elementTag Map.empty styleCss child

elAttrStyle' :: MonadWidget t m => Text -> Map Text Text -> Css -> m a -> m (El t, a)
elAttrStyle' elementTag attrs styleCss child =
    elAttr' elementTag (attrs <> "style" =: styleStr styleCss) child

elStyle :: MonadWidget t m => Text -> Css -> m a -> m a
elStyle elementTag styleCss child = elAttrStyle elementTag Map.empty styleCss child

elAttrStyle :: MonadWidget t m => Text -> Map Text Text -> Css -> m a -> m a
elAttrStyle elementTag attrs styleCss child =
    elAttr elementTag (attrs <> "style" =: styleStr styleCss) child

elDynAttrStyle' :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> Css -> m a
                -> m (El t, a)
elDynAttrStyle' elementTag dynAttrs styleCss child = do
    newAttrsDyn <- forDyn dynAttrs (<> "style" =: styleStr styleCss)
    elDynAttr' elementTag newAttrsDyn child

-- the tail . init removes leading and trailing {}
-- see https://github.com/sebastiaanvisser/clay/issues/120
styleStr :: Css -> Text
styleStr css = case TL.toStrict $ renderWith compact [] css of
    xs | T.length xs >= 3 -> T.tail $ T.init xs
    short                 -> short

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

smallIconButton :: MonadWidget t m => Text -> m (Event t ())
smallIconButton = iconButton 12

iconButton :: MonadWidget t m => Int -> Text -> m (Event t ())
iconButton iconHeight iconName = button' $
    elAttrStyle "img" ("src" =: getGlyphiconUrl iconName)
        (height $ px $ fromIntegral iconHeight) $ return ()

col :: MonadWidget t m => m a -> m a
col = elStyle "td" (paddingAll (px 5))

data ButtonInfo = PrimaryBtn Text | DangerBtn Text | NoBtn

setupModal :: MonadWidget t m => ModalLevel -> Event t a -> m (Event t b)
           -> m (Event t b)
setupModal modalLevel showEvent buildDialog = do
    showModalOnEvent modalLevel showEvent
    modalDyn <- holdDyn Nothing $ fmap Just showEvent
    dynModalVal <- forDyn modalDyn $ fmap (const buildDialog)
    readModalResult modalLevel dynModalVal

readModalResult :: MonadWidget t m => ModalLevel -> Dynamic t (Maybe (m (Event t a)))
                -> m (Event t a)
readModalResult modalLevel dynModalVal = do
    dynModalEvtEvt <- dynModal modalLevel dynModalVal
    dynModalDynEvt <- holdDyn never dynModalEvtEvt
    return (switch $ current dynModalDynEvt)

readDynMonadicEvent :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
readDynMonadicEvent dynMonadicEvent = do
    eventEvt <- dyn dynMonadicEvent
    dynEvt <- holdDyn never eventEvt
    return $ switch (current dynEvt)

buildModalBody :: MonadWidget t m => Text -> ButtonInfo
                 -> Dynamic t Text -> m a -> m (a, Event t (), Event t ())
buildModalBody title okBtnInfo dynErrMsg contents =
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

addErrorBox :: MonadWidget t m => Dynamic t Text -> m ()
addErrorBox dynErrMsg = do
    dynAttrs <- forDyn dynErrMsg $ \errMsg ->
        "class" =: "alert alert-danger"
        <> "role" =: "alert"
        <> attrStyleHideIf (T.null errMsg)
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

addOkButton :: MonadWidget t m => Text -> Text -> Bool -> m (El t)
addOkButton okBtnClass okBtnText isShow = do
    let styl = "style" =: if isShow then "" else "display: none"
    (okEl, _) <- elAttr' "button" ("type" =: "button"
                                   <> "class" =: ("btn btn-" <> okBtnClass)
                                   <> styl)
        $ text okBtnText
    return okEl

-- a secondary modal is on top of the basic one (modal in modal)
data ModalLevel = ModalLevelBasic | ModalLevelSecondary

topLevelModalId :: ModalLevel -> Text
topLevelModalId ModalLevelBasic = "toplevelmodal"
topLevelModalId ModalLevelSecondary = "toplevelsecmodal"

topLevelModalContentsId :: ModalLevel -> Text
topLevelModalContentsId ModalLevelBasic = "toplevelmodalcontents"
topLevelModalContentsId ModalLevelSecondary = "toplevelsecmodalcontents"

dynModal :: MonadWidget t m => ModalLevel -> Dynamic t (Maybe (m a)) -> m (Event t a)
dynModal modalLevel = dynAtEltId (topLevelModalContentsId modalLevel)

-- | this is copy-pasted & modified from 'dyn' from reflex-dom
-- instead of appending the nodes at the current position in the
-- DOM, append them under the node by the ID which you give.
    -- something with runImmediateDomBuilderT, see example Immediate.hs, line 184
dynAtEltId :: MonadWidget t m => Text -> Dynamic t (Maybe (m a)) -> m (Event t a)
dynAtEltId eltId child = undefined
    -- (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
    -- let e = fmap snd newChildBuilt
    -- childVoidAction <- hold never e
    -- performEvent_ $ fmap (const $ return ()) e
    -- addVoidAction $ switch childVoidAction
    -- doc <- askDocument
    -- let build = \case
    --         Nothing -> return ()
    --         Just c  -> do
    --             Just df <- liftIO $ createDocumentFragment doc
    --             Just docRoot <- liftIO $ getElementById doc eltId
    --             nodeLastChild <- liftIO $ getLastChild docRoot
    --             void $ liftIO $ replaceChild docRoot (Just df) nodeLastChild
    -- schedulePostBuild $ do
    --     c <- sample $ current child
    --     build c
    -- addVoidAction $ ffor (updated child) $ \newChild -> do
    --     build newChild
    -- return $ fmap fst newChildBuilt

rawPointerSpan :: MonadWidget t m => Dynamic t Text -> m ()
rawPointerSpan = rawSpan ("style" =: "cursor: pointer")

rawSpan :: MonadWidget t m => Map Text Text -> Dynamic t Text -> m ()
rawSpan attrs = void . elDynHtmlAttr' "span" attrs

getGlyphiconUrl :: Text -> Text
getGlyphiconUrl iconBase = "glyphicons_free/glyphicons/png/" <> iconBase <> ".png"

progressDialog :: MonadWidget t m => Dynamic t Int -> m ()
progressDialog percentDyn = do
    attrsDyn <- forDyn percentDyn $ \percent ->
        ("class" =: "progress" <> "value" =: T.pack (show percent) <> "max" =: "100")
    elDynAttr "progress" attrsDyn $ dynText =<< mapDyn ((<> "%") . T.pack . show) percentDyn

showModalOnEvent :: MonadWidget t m => ModalLevel -> Event t a -> m ()
showModalOnEvent modalLevel evt = performEvent_ $ fmap
    (const $ liftIO $ showModalIdDialog $ topLevelModalId modalLevel) evt

hideModalOnEvent :: MonadWidget t m => ModalLevel -> Event t a -> m ()
hideModalOnEvent modalLevel evt = performEvent_ $ fmap
    (const $ liftIO $ hideModalIdDialog $ topLevelModalId modalLevel) evt

combineDyns :: (Reflex t, MonadHold t m) => (b -> a -> b) -> b -> [Dynamic t a]
            -> m (Dynamic t b)
combineDyns _ item []   = return (constDyn item)
combineDyns f item rest = foldM (combineDyn f) (constDyn item) rest

data RemoteData a = RemoteDataInvalid Text | RemoteDataLoading | RemoteData a deriving Show

combineRemoteData :: (a -> b -> c) -> RemoteData a -> RemoteData b -> RemoteData c
combineRemoteData _ (RemoteDataInvalid x) _ = RemoteDataInvalid x
combineRemoteData _ _ (RemoteDataInvalid x) = RemoteDataInvalid x
combineRemoteData _ RemoteDataLoading _ = RemoteDataLoading
combineRemoteData _ _ RemoteDataLoading = RemoteDataLoading
combineRemoteData f (RemoteData x) (RemoteData y) = RemoteData (f x y)

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
    200 -> case _xhrResponse_responseText of
        Nothing -> RemoteData ()
        Just "" -> RemoteData ()
        Just x  -> RemoteDataInvalid $ "Expected empty response, got" <> x
    _ -> RemoteDataInvalid $ "HTTP response code " <> T.pack (show _xhrResponse_status)
             <> "; details: " <> fromMaybeEmpty "none" _xhrResponse_responseText

readRemoteData :: FromJSON a => XhrResponse -> RemoteData a
readRemoteData XhrResponse{..} = case _xhrResponse_status of
    200 -> case _xhrResponse_responseText of
        Nothing -> RemoteDataInvalid "Empty server response"
        Just rawData -> case decodeText rawData of
            Nothing -> RemoteDataInvalid $
                "JSON has invalid format: " <> fromMaybe "Nothing" _xhrResponse_responseText
            Just decoded -> RemoteData decoded
    _ -> RemoteDataInvalid $ "HTTP response code " <> T.pack (show _xhrResponse_status)
             <> "; details: " <> fromMaybeEmpty "none" _xhrResponse_responseText

fromMaybeEmpty :: (IsString a, Eq a) => a -> Maybe a -> a
fromMaybeEmpty val Nothing = val
fromMaybeEmpty val (Just "") = val
fromMaybeEmpty _ (Just r) = r

isRemoteDataLoading :: RemoteData a -> Bool
isRemoteDataLoading RemoteDataLoading = True
isRemoteDataLoading _ = False

remoteDataInvalidDesc :: RemoteData a -> Maybe Text
remoteDataInvalidDesc (RemoteDataInvalid x) = Just x
remoteDataInvalidDesc _ = Nothing

remoteDataErrorDescDyn :: MonadWidget t m => Event t (RemoteData a) -> m (Dynamic t Text)
remoteDataErrorDescDyn evt = holdDyn "" (fmapMaybe remoteDataInvalidDesc evt)

fromRemoteData :: RemoteData a -> Maybe a
fromRemoteData (RemoteData x) = Just x
fromRemoteData _ = Nothing

makeSimpleXhr :: (MonadWidget t m, FromJSON a) => Text -> Event t b
              -> m (Dynamic t (RemoteData a))
makeSimpleXhr url = makeSimpleXhr' (const url)

makeSimpleXhr' :: (MonadWidget t m, FromJSON a) => (b -> Text) -> Event t b
               -> m (Dynamic t (RemoteData a))
makeSimpleXhr' getUrl evt = do
    req <- performRequestAsync $ (\evtVal -> xhrRequest "GET" (getUrl evtVal) def) <$> evt
    holdDyn RemoteDataLoading $ fmap readRemoteData req
