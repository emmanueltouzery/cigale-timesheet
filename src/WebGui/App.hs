{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface #-}

import GHCJS.Types
import GHCJS.Foreign

import Reflex
import Reflex.Dom

import Clay
import Data.List
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad

import Common
import Config
import EventsView

foreign import javascript unsafe "window.location.hash.substr(1)" getLocationHash :: IO JSString
foreign import javascript unsafe "document.title = $1" setTitle :: JSString -> IO ()

css :: Css
css = do
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
        zIndex 2001

main :: IO ()
main = mainWidgetWithCss (T.encodeUtf8 $ TL.toStrict $ render css) cigaleView

cigaleView :: MonadWidget t m => m ()
cigaleView = do
    stylesheet "pikaday.css"
    stylesheet "bootstrap.min.css"
    liftIO (setTitle "Cigale timesheet")
    addModalDialogSkeleton 5000 ModalLevelBasic
    addModalDialogSkeleton 99999999 ModalLevelSecondary
    activeViewDyn <- navBar
    eventsView activeViewDyn
    configView activeViewDyn

-- | add the skeleton of a modal dialog that can be reused by
-- other parts of the application. They'll refer to the toplevelmodal
-- and toplevelmodalcontents DOM IDs.
addModalDialogSkeleton :: MonadWidget t m => Int -> ModalLevel -> m ()
addModalDialogSkeleton zIndex modalLevel =
    void $ elAttr' "div" ("class" =: "modal fade"
                          <> "tabindex" =: "-1"
                          <> "id" =: topLevelModalId modalLevel
                          <> "style" =: ("z-index: " <> show zIndex)) $
        elAttr "div" ("class" =: "modal-dialog"
                      <> "role" =: "document"
                      <> "id" =: topLevelModalContentsId modalLevel) $ text ""

data NavLinkItem = NavLinkItem
     {
         nliActiveView :: ActiveView,
         nliUrl :: String,
         nliDesc :: String
     }
navLinkItems :: [NavLinkItem]
navLinkItems =
    [
        NavLinkItem ActiveViewEvents "events" "Activities",
        NavLinkItem ActiveViewConfig "event-providers" "Event providers"
    ]

navBar :: MonadWidget t m => m (Dynamic t ActiveView)
navBar = do
    urlLocationHash <- liftIO $ fromJSString <$> getLocationHash
    rec
        viewEvts <-
            elAttr "div" ("role" =: "navigation" <> "style" =: "padding: 10px") $
                elAttr "ul" ("class" =: "nav nav-tabs") $ do
                    elAttr "span" ("class" =: "navbar-brand") $ text "Cigale"
                    e <- mapM (navLink activeViewDyn) navLinkItems
                    addAboutButton
                    return e
        let curView = fromMaybe ActiveViewEvents $
                          nliActiveView <$> find ((== urlLocationHash) . nliUrl) navLinkItems
        activeViewDyn <- holdDyn curView $ leftmost viewEvts
    return activeViewDyn

addAboutButton :: MonadWidget t m => m ()
addAboutButton = do
    (img, _) <- elAttr' "img" ("src" =: getGlyphiconUrl "glyphicons-195-question-sign"
                              <> "class" =: "pull-xs-right"
                              <> "style" =: "cursor: pointer") $ return ()
    void $ setupModal ModalLevelBasic (domEvent Click img) $ do
        void $ buildModalBody "About" NoBtn
            (constDyn "") (text "Icons from "
                   >> elAttr "a" ("href" =: "http://glyphicons.com") (text "Glyphicons")
                   >> text ", under a "
                   >> elAttr "a" ("href" =: "http://creativecommons.org/licenses/by/3.0/")
                       (text "CC BY 3.0 license."))
        return never

navLink :: MonadWidget t m => Dynamic t ActiveView -> NavLinkItem -> m (Event t ActiveView)
navLink activeViewDyn NavLinkItem{..} = do
    attrs <- forDyn activeViewDyn $ \curView ->
        "href" =: ("#" <> nliUrl) <> "style" =: "outline: 0" <>
            attrOptDyn "class" "active" (curView == nliActiveView) "nav-link"
    (a, _) <- elAttr "li" ("class" =: "nav-item") $
        elDynAttr' "a" attrs $ text nliDesc
    return $ fmap (const nliActiveView) $ domEvent Click a
