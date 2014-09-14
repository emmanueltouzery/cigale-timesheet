{-# LANGUAGE EmptyDataDecls #-}

-- the original code is from: https://github.com/jorpic/fay-knockout
-- I've just updated it to work with recent versions of Fay and added
-- a couple of helpers.

module Knockout
  ( Observable
  , koObservable
  , koComputed
  , koGet
  , koSet
  , koSubscribe

  , ObservableList
  , koComputedList
  , koSetList
  , koObservableList
  , koPushObservableList
  , koPushAllObservableList
  , koUnwrapObservableList
  , koRemoveAllObservableList
  , koRemoveObservableList
  , koReplaceElementObservableList
  , koFilter
  , koFilterM
  , koSort
  , (~>)
  , (~~>)

  , KnockoutModel
  , koApplyBindings
  , koApplyBindingsSubTree
  ) where


import Prelude
import FFI
import Utils
import JQuery hiding (not)

import Fay.Text (fromString, Text, unpack)

data Observable a

koObservable :: Automatic a -> Observable a
koObservable = ffi "ko.observable(%1)"

koComputed :: Fay (Automatic a) -> Observable a
koComputed = ffi "ko.computed(%1)"

koComputedList :: Fay [Automatic a] -> ObservableList a
koComputedList = ffi "ko.computed(%1)"

koGet :: Observable a -> Fay a
koGet = ffi "%1()"

koSet :: Observable a -> Automatic a -> Fay ()
koSet = ffi "%1(%2)"

koSubscribe :: Observable a -> (a -> Fay ()) -> Fay ()
koSubscribe = ffi "%1.subscribe(%2)"

(~>) :: Automatic a -> Observable a -> Fay ()
(~>) = flip koSet

-- In real haskell, with immutability, i would not
-- need that. But in fay I apparently do...
(~~>) :: Automatic a -> Observable a -> Fay ()
(~~>) = ffi "%2(JSON.parse(JSON.stringify(%1)))"

data ObservableList a

-- TODO make a typeclass for Observable and ObservableList?
koSetList :: ObservableList a -> Automatic [a] -> Fay ()
koSetList = ffi "%1(%2)"

koObservableList :: Automatic [a] -> Fay (ObservableList a)
koObservableList = ffi "ko.observableArray(%1)"

koPushObservableList :: ObservableList a -> Automatic a -> Fay ()
koPushObservableList = ffi "%1.push(%2)"

koPushAllObservableList :: ObservableList a -> Automatic [a] -> Fay ()
koPushAllObservableList = foldM_ (\soFar x -> koPushObservableList soFar x >> return soFar)

koRemoveAllObservableList :: ObservableList a -> Fay ()
koRemoveAllObservableList = ffi "%1.removeAll()"

koUnwrapObservableList :: ObservableList a -> Fay [a]
koUnwrapObservableList = ffi "%1()"

koRemoveObservableList :: ObservableList a -> a -> Fay ()
koRemoveObservableList = ffi "%1.remove(%2)"

koReplaceElementObservableList :: ObservableList a -> a -> a -> Fay ()
koReplaceElementObservableList = ffi "%1.splice(%1.indexOf(%2), 1, %3)"

koObservableListRemove :: (a -> bool) -> ObservableList a -> Fay (ObservableList a)
koObservableListRemove = ffi "%2.remove(%1)"

koFilter :: (a -> Bool) -> ObservableList a -> Fay (ObservableList a)
koFilter f = koObservableListRemove (not . f)

koSort :: (a -> Text) -> ObservableList a -> Fay (ObservableList a)
koSort f = koSort_ (\x y -> case strComp (unpack $ f x) (unpack $ f y) of { GT -> 1; LT -> -1; EQ -> 0;})

koSort_ :: (a -> a -> Int) -> ObservableList a -> Fay (ObservableList a)
koSort_ = ffi "%2.sort(%1)"

koObservableListRemoveM :: (a -> Fay bool) -> ObservableList a -> Fay (ObservableList a)
koObservableListRemoveM = ffi "%2.remove(%1)"

koFilterM :: (a -> Fay Bool) -> ObservableList a -> Fay (ObservableList a)
koFilterM f = koObservableListRemoveM (\x -> not <$> f x)

class KnockoutModel m

koApplyBindings :: KnockoutModel m => Automatic m -> Fay ()
koApplyBindings = ffi "ko.applyBindings(%1)"

koApplyBindingsSubTree :: KnockoutModel m => Automatic m -> JQuery -> Fay ()
koApplyBindingsSubTree = ffi "ko.applyBindings(%1, %2)"
