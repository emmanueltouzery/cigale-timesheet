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
  , (~>)

  , KnockoutModel
  , koApplyBindings
  , koApplyBindingsSubTree
  ) where


import Prelude
import FFI
import Utils
import JQuery

import Fay.Text (fromString, Text)

data Observable a

koObservable :: a -> Observable a
koObservable = ffi "ko.observable(%1)"

koComputed :: Fay a -> Observable a
koComputed = ffi "ko.computed(%1)"

koComputedList :: Fay [a] -> ObservableList a
koComputedList = ffi "ko.computed(%1)"

koGet :: Observable a -> Fay a
koGet = ffi "%1()"

koSet :: Observable a -> Automatic a -> Fay ()
koSet = ffi "%1(%2)"

(~>) :: Automatic a -> Observable a -> Fay ()
(~>) = flip koSet

data ObservableList a

-- TODO make a typeclass for Observable and ObservableList?
koSetList :: ObservableList a -> Automatic [a] -> Fay ()
koSetList = ffi "%1(%2)"

koObservableList :: [a] -> Fay (ObservableList a)
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

class KnockoutModel m

koApplyBindings :: KnockoutModel m => Automatic m -> Fay ()
koApplyBindings = ffi "ko.applyBindings(%1)"

koApplyBindingsSubTree :: KnockoutModel m => Automatic m -> JQuery -> Fay ()
koApplyBindingsSubTree = ffi "ko.applyBindings(%1, %2)"
