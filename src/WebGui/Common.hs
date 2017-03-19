{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, JavaScriptFFI, FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, OverloadedStrings, TypeFamilies, RecursiveDo #-}

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

import Control.Monad.State.Class (modify)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Control.Lens hiding (element)
import Clay as C hiding (filter, title, contents, action, url, (&), placeholder, id, reverse, none)
import qualified Clay as C
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.Trans
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

foreign import javascript unsafe "$($1).modal('show')" _showModalDialog :: JSVal -> IO ()
showModalDialog :: El t -> IO ()
showModalDialog = _showModalDialog . unNode . toNode . _element_raw

foreign import javascript unsafe "$($1).modal('hide')" _hideModalDialog :: JSVal -> IO ()
hideModalDialog :: El t -> IO ()
hideModalDialog = _hideModalDialog . unNode . toNode . _element_raw

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

attrOptDyn :: a -> Text -> Bool -> Text -> Map a Text
attrOptDyn attrib opt isShow str = attrib =: (str <> if isShow then " " <> opt else "")

attrStyleWithHideIf :: Bool -> Css -> Map Text Text
attrStyleWithHideIf isHide rest = "style" =: styleStr (styleWithHideIf isHide rest)

styleWithHideIf :: Bool -> Css -> Css
styleWithHideIf isHide rest = rest >> when isHide (display C.none)

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

button' :: MonadWidget t m => m a -> m (El t, Event t ())
button' contents = do
    -- forcing white background, otherwise it stays to gray after
    -- being pressed which I find ugly.
    (e, _) <- elAttrStyle' "button"
        ("class" =: "btn btn-secondary btn-sm") (backgroundColor white) contents
    return (e, domEvent Click e)

smallIconButton :: MonadWidget t m => Text -> m (El t, Event t ())
smallIconButton = iconButton 12

iconButton :: MonadWidget t m => Int -> Text -> m (El t, Event t ())
iconButton iconHeight iconName = button' $
    elAttrStyle "img" ("src" =: getGlyphiconUrl iconName)
        (height $ px $ fromIntegral iconHeight) $ return ()

col :: MonadWidget t m => m a -> m a
col = elStyle "td" (paddingAll (px 5))

data ButtonInfo = PrimaryBtn Text | DangerBtn Text | NoBtn

readDynMonadicEvent :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
readDynMonadicEvent dynMonadicEvent = do
    eventEvt <- dyn dynMonadicEvent
    dynEvt   <- holdDyn never eventEvt
    return $ switch (current dynEvt)

wrapInModalDialogSkeleton :: MonadWidget t m => Event t b -> Int -> m a -> m (El t, a)
wrapInModalDialogSkeleton showEvt zIndexVal contents = do
    (elt, r) <- elAttrStyle' "div"
        ("class"    =: "modal fade" <> "tabindex" =: "-1")
        (zIndex $ fromIntegral zIndexVal) $
            elAttr "div" ("class" =: "modal-dialog" <>
                          "role"  =: "document") $ contents
    performEvent_ $ (const $ liftIO $ showModalDialog elt) <$> showEvt
    return (elt, r)

buildModalBody :: MonadWidget t m => Event t x -> Text -> ButtonInfo
                 -> Dynamic t Text -> Dynamic t (m a) -> m (Dynamic t a, Event t (), Event t ())
buildModalBody showEvt title okBtnInfo dynErrMsg contentsDyn = do
    (modalElt, r) <- wrapInModalDialogSkeleton showEvt 5000
                    (buildModalBody' showEvt title okBtnInfo dynErrMsg contentsDyn)
    performEvent_ $ (const $ liftIO $ hideModalDialog modalElt) <$> view _2 r -- TODO this is wrong, maybe comm with the server, must wait for success
    performEvent_ $ (const $ liftIO $ hideModalDialog modalElt) <$> view _3 r
    return r

buildModalBody' :: MonadWidget t m => Event t x -> Text -> ButtonInfo
                 -> Dynamic t Text -> Dynamic t (m a) -> m (Dynamic t a, Event t (), Event t ())
buildModalBody' showEvt title okBtnInfo dynErrMsg contentsDyn =
    elAttr "div" ("class" =: "modal-content") $ do
        elAttr "div" ("class" =: "modal-header") $ do
            let crossBtnAttrs = "type" =: "button" <> "class" =: "close"
                    <> "data-dismiss" =: "modal" <> "aria-label" =: "Close"
            void $ elAttr "button" crossBtnAttrs $
                elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
            elAttr "h4" ("class" =: "modal-title") $ text title
        bodyRes <- elAttr "div" ("class" =: "modal-body") $ do
            addErrorBox dynErrMsg
            initial <- sample (current contentsDyn)
            widgetHold initial (updated contentsDyn)
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
            let closeBtnAttrs = "type" =: "button"
                    <> "class" =: "btn btn-secondary"
            (closeEl, _) <- elAttr' "button" closeBtnAttrs $ text "Close"
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

-- | this is copy-pasted & modified from 'dyn' from reflex-dom
-- instead of appending the nodes at the current position in the
-- DOM, append them under the node by the ID which you give.
-- TODO move to reflex-dom 'placeholder' mechanism?
    -- something with runImmediateDomBuilderT, see example Immediate.hs, line 184
-- dynAtEltId :: MonadWidget t m => El t -> Text -> Dynamic t (Maybe (m a)) -> m (Event t a)
-- dynAtEltId elt eltId child = do
--     (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
--     let e = fmap snd newChildBuilt
--     childVoidAction <- hold never e
--     performEvent_ $ fmap (const $ return ()) e
--     addVoidAction $ switch childVoidAction
--     (Just doc) <- getOwnerDocument (_el_element elt)
--     let build = \case
--             Nothing -> return ()
--             Just c  -> do
--                 Just df <- liftIO $ createDocumentFragment doc
--                 Just docRoot <- liftIO $ getElementById doc eltId
--                 nodeLastChild <- liftIO $ getLastChild docRoot
--                 void $ liftIO $ replaceChild docRoot (Just df) nodeLastChild
--     schedulePostBuild $ do
--         c <- sample $ current child
--         build c
--     addVoidAction $ ffor (updated child) $ \newChild -> do
--         build newChild
--     return $ fmap fst newChildBuilt

-- | Given a Dynamic of widget-creating actions, create a widget that is recreated whenever the Dynamic updates.
--   The returned Event of widget results occurs when the Dynamic does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened (via 'switchPromptly').


-- dynAtEltId :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
-- dynAtEltId child = do
--   postBuild <- getPostBuild
--   let newChild = leftmost [updated child, tag (current child) postBuild]
--   snd <$> widgetHoldInternal' (return ()) newChild

-- widgetHoldInternal' :: DomBuilder t m => m a -> Event t (m b) -> m (a, Event t b)
-- widgetHoldInternal' child0 child' = do
--   childResult0 <- deletable (void child') child0
--   childResult' <- liftPlaceholder'' $ def & placeholderConfig_insertAbove .~ fmap (deletable (void child')) child'
--   return (childResult0, _placeholder_insertedAbove childResult')

-- liftPlaceholder'' cfg = liftWithStateless $ \run -> placeholder'' $ fmap1 run cfg

-- placeholder'' :: (DomBuilderSpace (PostBuildT t m) ~ DomBuilderSpace m) => (PlaceholderConfig above t m) -> m (Placeholder above t)
-- placeholder'' cfg = lift $ do
--   rec childPostBuild <- deletable (_placeholder_deletedSelf p) $ performEvent $ return () <$ _placeholder_insertedAbove p
--       p <- placeholder $ cfg
--         { _placeholderConfig_insertAbove = ffor (_placeholderConfig_insertAbove cfg) $ \a -> runPostBuildT a =<< headE childPostBuild
--         }
--   return p

-- -- placeholder' cfg = do
-- --   let cfg' = cfg
-- --         { _placeholderConfig_insertAbove = runDynamicWriterTInternal <$> _placeholderConfig_insertAbove cfg
-- --         }
-- --   let manageChildren :: Event t (NonEmpty (Replaceable t (Dynamic t w))) -- ^ Add nodes on the right; these are in reverse order
-- --                      -> Event t () -- ^ No more nodes will be added after this event fires
-- --                      -> m (Replaceable t (Dynamic t w))
-- --       manageChildren newChildren additionsCeased = do
-- --         rec nextId <- hold (0 :: Int) newNextId -- We assume this will never wrap around
-- --             let numberedNewChildren :: Event t (Int, PatchMap (Map Int (Replaceable t (Dynamic t w))))
-- --                 numberedNewChildren = flip pushAlways newChildren $ \rcs -> do
-- --                   let cs = reverse $ toList rcs
-- --                   myFirstId <- sample nextId
-- --                   let (myNextId, numbered) = mapAccumL (\n v -> (succ n, (n, Just v))) myFirstId cs
-- --                   return (myNextId, PatchMap $ Map.fromList numbered)
-- --                 newNextId = fst <$> numberedNewChildren
-- --         mconcatIncrementalReplaceableDynMap Map.empty (snd <$> numberedNewChildren) additionsCeased
-- --   rec children <- lift $ manageChildren childOutputs $ cfg ^. deleteSelf
-- --       p <- DynamicWriterT $ do
-- --         modify (children:)
-- --         lift $ placeholder cfg'
-- --       let result = fst <$> _placeholder_insertedAbove p
-- --           childOutputs = fmapMaybe (nonEmpty . snd) $ _placeholder_insertedAbove p
-- --   return $ p
-- --     { _placeholder_insertedAbove = result
-- --     }

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

instance Monoid a => Monoid (RemoteData a) where
    mappend = combineRemoteData mappend
    mempty = RemoteData mempty
    -- mappend (RemoteDataInvalid x) _ = RemoteDataInvalid x
    -- mappend RemoteDataLoading _ = RemoteDataLoading
    -- mappend (RemoteData a) (RemoteData b) = RemoteData (mappend a b)

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
