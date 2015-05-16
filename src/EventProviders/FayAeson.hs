{-# LANGUAGE OverloadedStrings #-}

-- see https://github.com/faylang/fay/issues/311

module FayAeson where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

addInstance :: T.Text -> Value -> Value
addInstance dataName (Object params) = Object (H.insert "instance" (String dataName) params)
addInstance _ x = x
