{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, JavaScriptFFI, ForeignFunctionInterface #-}

import GHCJS.Types
import GHCJS.Foreign

import Reflex
import Reflex.Dom

import Data.List
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

-- url is http://localhost:8000/static/index.html
-- start cigale with .stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/cigale-timesheet

-- TODO migrate to some haskell CSS DSL, like clay?
css :: ByteString
css = BS.intercalate "\n"
      [
          ".ellipsis { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }",
          "#pleasehold { position: absolute; width: 200px; height: 50px; background: aliceblue; text-align: center; top: 50%; left: 50%; margin-left: -100px; margin-top: -25px; line-height: 50px; z-index: 2001; }"
      ]

main :: IO ()
main = mainWidgetWithCss css cigaleView

cigaleView :: MonadWidget t m => m ()
cigaleView = do
    stylesheet "pikaday.css"
    stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.2/css/bootstrap.min.css"
    activeViewDyn <- navBar
    -- add the skeleton of a modal dialog that can be reused by
    -- other parts of the application. They'll refer to the toplevelmodal
    -- and toplevelmodalcontents DOM IDs.
    void $ elAttr' "div" ("class" =: "modal fade"
                          <> "tabindex" =: "-1"
                          <> "id" =: topLevelModalId
                          <> "style" =: "z-index: 99999999") $
        elAttr "div" ("class" =: "modal-dialog"
                      <> "role" =: "document"
                      <> "id" =: topLevelModalContentsId) $ text ""
    eventsView activeViewDyn
    configView activeViewDyn

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
            elAttr "nav" ("class" =: "navbar navbar-light bg-faded"
                          <> "style" =: "margin-right: 10px; margin-bottom: 10px;") $
                elAttr "div" ("class" =: "nav navbar-nav") $ do
                    elAttr "a" ("href" =: "#events" <> "class" =: "navbar-brand") $ text "Cigale"
                    mapM (navLink activeViewDyn) navLinkItems
        let curView = fromMaybe ActiveViewEvents $
                          nliActiveView <$> find ((== urlLocationHash) . nliUrl) navLinkItems
        activeViewDyn <- holdDyn curView $ leftmost viewEvts
    return activeViewDyn

navLink :: MonadWidget t m => Dynamic t ActiveView -> NavLinkItem -> m (Event t ActiveView)
navLink activeViewDyn NavLinkItem{..} = do
    attrs <- mapDyn (\curView -> "href" =: ("#" <> nliUrl)
                                 <> attrOptDyn "class" "active" (curView == nliActiveView) "nav-item nav-link") activeViewDyn
    (a, _) <- elDynAttr' "a" attrs $ text nliDesc
    return $ fmap (const nliActiveView) $ domEvent Click a
