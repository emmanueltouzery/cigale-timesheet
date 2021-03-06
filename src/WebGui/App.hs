{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

import GHCJS.Types
import GHCJS.DOM.Types (fromJSString)

import Reflex
import Reflex.Dom hiding (display, fromJSString)

import Clay
import Data.List
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Monoid
import Control.Monad.IO.Class
import Data.Maybe

import Common
import Config
import EventsView

foreign import javascript unsafe "window.location.hash.substr(1)" getLocationHash_ :: IO JSString
foreign import javascript unsafe "document.title = $1" setTitle :: JSString -> IO ()

getLocationHash :: IO Text
getLocationHash = fromJSString <$> getLocationHash_

css :: Css
css = do
    html ? do
        height (pct 100)
        marginAll (px 0)
        paddingAll (px 0)
    body ? do
        "flex" -: "1"
        height (pct 100)
        marginAll (px 0)
        paddingAll (px 0)
        display flex
        flexDirection column
    ".ellipsis" ? do
        whiteSpace nowrap
        overflow hidden
        "text-overflow" -: "ellipsis"
    "#pleasehold" ? do
        position absolute
        width (px 200)
        height (px 50)
        background aliceblue
        textAlign (alignSide sideCenter)
        top (pct 50)
        left (pct 50)
        marginLeft (px (-100))
        marginTop (px (-25))
        lineHeight (px 50)
        zIndex 100000

-- the highlighting in the pikaday date picker for days
-- which have events (which I use for days for which we
-- already prefetched events) is too flashy for me.
-- reduce it only to underline.
overridePikaday :: TL.Text
overridePikaday = "\ntd[class=has-event] .pika-button:not(:hover)  {\
    \box-shadow: none !important; color: #666 !important;\
    \font-weight: normal !important; background: #f5f5f5 !important;\
    \text-decoration: underline !important};"

main :: IO ()
main = mainWidgetWithCss (T.encodeUtf8 $ TL.toStrict $ render css <> overridePikaday) cigaleView

cigaleView :: MonadWidget t m => m ()
cigaleView = do
    stylesheet "pikaday.css"
    stylesheet "bootstrap.min.css"
    liftIO (setTitle "Cigale timesheet")
    activeViewDyn <- navBar
    eventsView activeViewDyn
    configView activeViewDyn

data NavLinkItem = NavLinkItem
     {
         nliActiveView :: ActiveView,
         nliUrl        :: Text,
         nliDesc       :: Text
     }
navLinkItems :: [NavLinkItem]
navLinkItems =
    [
        NavLinkItem ActiveViewEvents "events" "Activities",
        NavLinkItem ActiveViewConfig "event-providers" "Event providers"
    ]

navBar :: MonadWidget t m => m (Dynamic t ActiveView)
navBar = do
    urlLocationHash <- liftIO getLocationHash
    rec
        viewEvts <-
            elAttrStyle "div" ("role" =: "navigation") (marginAll (px 10) >> flexShrink 0) $
                elAttr "ul" ("class" =: "nav nav-tabs") $ do
                    elAttr "span" ("class" =: "navbar-brand") $ text "Cigale"
                    e <- mapM (navLink activeViewDyn) navLinkItems
                    elAttr "li" ("class" =: "nav-item ml-auto") addAboutButton
                    return e
        let curView = fromMaybe ActiveViewEvents $
                          nliActiveView <$> find ((== urlLocationHash) . nliUrl) navLinkItems
        activeViewDyn <- holdDyn curView $ leftmost viewEvts
    return activeViewDyn

addAboutButton :: MonadWidget t m => m ()
addAboutButton = do
    let iconUrl = getGlyphiconUrl "glyphicons-195-question-sign"
    (image, _) <- elAttrStyle' "img"
        ("src" =: iconUrl <> "class" =: "navbar-text") (cursor pointer) $ return ()
    let modalCts =
            text "Icons from "
            >> elAttr "a" ("href" =: "http://glyphicons.com") (text "Glyphicons")
            >> text ", under a "
            >> elAttr "a" ("href" =: "http://creativecommons.org/licenses/by/3.0/") (text "CC BY 3.0 license.")
    (dlgInfo, dlgClose) <-
        buildModalBody (domEvent Click image) "About" NoBtn (constDyn "") (constDyn modalCts)
    performEvent_ $ const (liftIO dlgClose) <$> dlgOkEvt dlgInfo

navLink :: MonadWidget t m => Dynamic t ActiveView -> NavLinkItem -> m (Event t ActiveView)
navLink activeViewDyn NavLinkItem{..} = do
    let attrs = ffor activeViewDyn $ \curView ->
          "href" =: ("#" <> nliUrl) <>
            attrOptDyn "class" "active" (curView == nliActiveView) "nav-link"
    (lnk, _) <- elAttr "li" ("class" =: "nav-item") $
        elDynAttrStyle' "a" attrs ("outline" -: "0") $ text nliDesc
    return $ const nliActiveView <$> domEvent Click lnk
