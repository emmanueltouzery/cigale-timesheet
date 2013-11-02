{-# LANGUAGE EmptyDataDecls #-}

-- the original code is from: https://github.com/jorpic/fay-knockout
-- I've just updated it to work with recent versions of Fay and added
-- a couple of helpers.

module Knockout
  ( Observable
  , ko_observable
  , ko_computed
  , ko_get
  , ko_set

  , ObservableArray
  , ko_observableList
  , ko_pushObservableArray
  , ko_pushAllObservableArray
  , ko_unwrapObservableArray
  , ko_removeAllObservableArray
  , ko_removeObservableArray
  , ko_replaceElementObservableArray
  , (~>)

  , KnockoutModel
  , ko_applyBindings
  ) where


import Prelude
import FFI
import Utils

import Fay.Text (fromString, Text)

data Observable a

ko_observable :: a -> Observable a
ko_observable = ffi "ko.observable(%1)"

ko_computed :: Fay a -> Observable a
ko_computed = ffi "ko.computed(%1)"

ko_get :: Observable a -> Fay a
ko_get = ffi "%1()"

ko_set :: Observable a -> Automatic a -> Fay ()
ko_set = ffi "%1(%2)"

(~>) :: Automatic a -> Observable a -> Fay ()
(~>) = flip ko_set

data ObservableArray a

ko_observableList :: [a] -> Fay (ObservableArray a)
ko_observableList = ffi "ko.observableArray(%1)"

ko_pushObservableArray :: ObservableArray a -> Automatic a -> Fay ()
ko_pushObservableArray = ffi "%1.push(%2)"

ko_pushAllObservableArray :: ObservableArray a -> Automatic [a] -> Fay ()
ko_pushAllObservableArray list = foldM_ (\soFar x -> ko_pushObservableArray soFar x >> return soFar) list

ko_removeAllObservableArray :: ObservableArray a -> Fay ()
ko_removeAllObservableArray = ffi "%1.removeAll()"

ko_unwrapObservableArray :: ObservableArray a -> Fay [a]
ko_unwrapObservableArray = ffi "%1()"

ko_removeObservableArray :: ObservableArray a -> a -> Fay ()
ko_removeObservableArray = ffi "%1.remove(%2)"

ko_replaceElementObservableArray :: ObservableArray a -> a -> a -> Fay ()
ko_replaceElementObservableArray = ffi "%1.splice(%1.indexOf(%2), 1, %3)"

class KnockoutModel m

ko_applyBindings :: KnockoutModel m => Automatic m -> Fay ()
ko_applyBindings = ffi "ko.applyBindings(%1)"

