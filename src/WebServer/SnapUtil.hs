{-# LANGUAGE OverloadedStrings #-}
module SnapUtil where

import Snap.Core
import Data.ByteString (ByteString)
import Control.Error
import Control.Monad.Trans
import Data.Monoid

setResponse :: Either ByteString ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val

setActionResponse :: ExceptT ByteString Snap ByteString -> Snap ()
setActionResponse action = runExceptT action >>= setResponse

hParam :: ByteString -> ExceptT ByteString Snap ByteString
hParam t = lift (getParam t) >>= hoistEither . note ("Parameter missing: " <> t)

noteET :: Monad m => a -> Maybe b -> ExceptT a m b
noteET l = hoistEither . note l
