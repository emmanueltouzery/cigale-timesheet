{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, JavaScriptFFI, FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, TypeFamilies, TemplateHaskell #-}

module Common where

import GHCJS.Types
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Types hiding (Text, Event)

import Reflex.Dom hiding (display)
import Data.String

import Clay as C hiding (filter, title, contents, action, url, (&),
                         placeholder, id, reverse, none, initial, a, b)
import qualified Clay as C
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
import Control.Lens

data ActiveView = ActiveViewEvents | ActiveViewConfig deriving (Eq, Show)

unwrapElt :: El t -> JSVal
unwrapElt = unNode . toNode . _element_raw

foreign import javascript unsafe "$($1).modal('show')" _showModalDialog :: JSVal -> IO ()
showModalDialog :: El t -> IO ()
showModalDialog = _showModalDialog . unwrapElt

foreign import javascript unsafe "$($1).modal('hide')" _hideModalDialog :: JSVal -> IO ()
hideModalDialog :: El t -> IO ()
hideModalDialog = _hideModalDialog . unwrapElt

eltStripClass :: IsElement self => self -> Text -> IO ()
eltStripClass elt className = do
    curClasses <- T.splitOn " " . T.pack <$> getClassName elt
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
    let newAttrsDyn = ffor dynAttrs (<> "style" =: styleStr styleCss)
    elDynAttr' elementTag newAttrsDyn child

elDynAttrStyle :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> Css -> m a -> m a
elDynAttrStyle elementTag dynAttrs styleCss child =
    snd <$> elDynAttrStyle' elementTag dynAttrs styleCss child

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
        ("class" =: "modal fade" <> "tabindex" =: "-1")
        (zIndex $ fromIntegral zIndexVal) $
            elAttr "div" ("class" =: "modal-dialog" <>
                          "role"  =: "document") contents
    performEvent_ $ const (liftIO $ showModalDialog elt) <$> showEvt
    return (elt, r)

data ModalBody t a = ModalBody
    { dlgContentsDyn :: Dynamic t a
    , dlgOkEvt       :: Event t ()
    , dlgCloseEvt    :: Event t ()
    }

buildModalBody :: MonadWidget t m => Event t x -> Text -> ButtonInfo
               -> Dynamic t Text -> Dynamic t (m a) -> m (ModalBody t a, IO ())
buildModalBody showEvt title okBtnInfo dynErrMsg contentsDyn = do
    (modalElt, r) <- wrapInModalDialogSkeleton showEvt 5000
                    (buildModalBody' title okBtnInfo dynErrMsg contentsDyn)
    let hideModal = hideModalDialog modalElt
    performEvent_ $ const (liftIO hideModal) <$> dlgCloseEvt r
    return (r, hideModal)

buildModalBody' :: MonadWidget t m => Text -> ButtonInfo
                 -> Dynamic t Text -> Dynamic t (m a) -> m (ModalBody t a)
buildModalBody' title okBtnInfo dynErrMsg contentsDyn =
    elAttr "div" ("class" =: "modal-content") $ do
        elAttr "div" ("class" =: "modal-header") $ do
            elAttr "h5" ("class" =: "modal-title") $ text title
            let crossBtnAttrs = "type" =: "button" <> "class" =: "close"
                    <> "data-dismiss" =: "modal" <> "aria-label" =: "Close"
            void $ elAttr "button" crossBtnAttrs $
                elDynHtmlAttr' "span" ("aria-hidden" =: "true") (constDyn "&times;")
        bodyRes <- elAttr "div" ("class" =: "modal-body") $ do
            addErrorBox dynErrMsg
            initial <- sample (current contentsDyn)
            widgetHold initial (updated contentsDyn)
        (okEvt, closeEvt)  <- addModalFooter okBtnInfo
        return (ModalBody bodyRes okEvt closeEvt)

addErrorBox :: MonadWidget t m => Dynamic t Text -> m ()
addErrorBox dynErrMsg = do
    let dynAttrs = ffor dynErrMsg $ \errMsg ->
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

rawPointerSpan :: MonadWidget t m => Dynamic t Text -> m ()
rawPointerSpan = rawSpan ("style" =: "cursor: pointer")

rawSpan :: MonadWidget t m => Map Text Text -> Dynamic t Text -> m ()
rawSpan attrs = void . elDynHtmlAttr' "span" attrs

getGlyphiconUrl :: Text -> Text
getGlyphiconUrl iconBase = "glyphicons_free/glyphicons/png/" <> iconBase <> ".png"

combineDyns :: Reflex t => (b -> a -> b) -> b -> [Dynamic t a] -> Dynamic t b
combineDyns _ item []   = constDyn item
combineDyns f item rest = foldl' (zipDynWith f) (constDyn item) rest

data RemoteData a = RemoteDataInvalid Text | RemoteDataLoading | RemoteData a deriving Show
$(makePrisms ''RemoteData)

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

remoteDataErrorDescDyn :: MonadWidget t m => Event t (RemoteData a) -> m (Dynamic t Text)
remoteDataErrorDescDyn evt = holdDyn "" (fmapMaybe (preview _RemoteDataInvalid) evt)

fromRemoteData :: RemoteData a -> Maybe a
fromRemoteData = preview _RemoteData

makeSimpleXhr :: (MonadWidget t m, FromJSON a) => Text -> Event t b
              -> m (Dynamic t (RemoteData a))
makeSimpleXhr url = makeSimpleXhr' (const url)

makeSimpleXhr' :: (MonadWidget t m, FromJSON a) => (b -> Text) -> Event t b
               -> m (Dynamic t (RemoteData a))
makeSimpleXhr' getUrl evt = do
    req <- performRequestAsync $ (\evtVal -> xhrRequest "GET" (getUrl evtVal) def) <$> evt
    holdDyn RemoteDataLoading $ fmap readRemoteData req

displayLoadingThrobber :: MonadWidget t m => Dynamic t (RemoteData a) -> m ()
displayLoadingThrobber respDyn = do
    let holdAttrs = ffor respDyn $ \resp ->
          "id" =: "pleasehold" <> attrStyleHideIf (not $ isRemoteDataLoading resp)
    void $ elDynAttr "div" holdAttrs $ text "Please hold..."

-- hopelessly naive implementation
zip3DynWith :: Reflex t =>
  (a -> b -> c -> d) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> Dynamic t d
zip3DynWith f d1 d2 d3 = zipDynWith (\a (b,c) -> f a b c) d1 (zipDynWith (,) d2 d3)
