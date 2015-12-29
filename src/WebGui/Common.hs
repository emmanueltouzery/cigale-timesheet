{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings #-}

module Common where

import GHCJS.DOM.Element

import Reflex.Dom
import Data.Dependent.Sum (DSum ((:=>)))

import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Map (Map)
import Control.Monad.IO.Class

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

data RemoteData a = RemoteDataInvalid | RemoteDataLoading | RemoteData a

instance Functor RemoteData where
    fmap _ RemoteDataLoading = RemoteDataLoading
    fmap _ RemoteDataInvalid = RemoteDataInvalid
    fmap f (RemoteData a) = RemoteData (f a)

instance Applicative RemoteData where
    pure = RemoteData
    RemoteData f <*> r = fmap f r
    RemoteDataInvalid <*> _ = RemoteDataInvalid
    RemoteDataLoading <*> _ = RemoteDataLoading

instance Monad RemoteData where
    RemoteDataInvalid >>= _ = RemoteDataInvalid
    RemoteDataLoading >>= _ = RemoteDataLoading
    RemoteData x >>= f = f x

readRemoteData :: Maybe a -> RemoteData a
readRemoteData (Just x) = RemoteData x
readRemoteData Nothing = RemoteDataInvalid

isRemoteDataLoading :: RemoteData a -> Bool
isRemoteDataLoading RemoteDataLoading = True
isRemoteDataLoading _ = False
