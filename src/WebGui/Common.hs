{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings #-}

module Common where

import GHCJS.DOM.Element

import Reflex.Dom

import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Monoid
import Data.Map (Map)

data ActiveView = ActiveViewEvents | ActiveViewConfig deriving (Eq, Show)

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
