{-# LANGUAGE OverloadedStrings #-}
module SnapUtil where

import Snap.Core
import qualified Data.ByteString as BS
import Control.Error
import Control.Monad.Trans

setResponse :: Either BS.ByteString BS.ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val

setActionResponse :: ExceptT BS.ByteString Snap BS.ByteString -> Snap ()
setActionResponse action = runExceptT action >>= setResponse

hParam :: BS.ByteString -> ExceptT BS.ByteString Snap BS.ByteString
hParam t = lift (getParam t) >>= hoistEither . note (BS.append "Parameter missing: " t)

noteET :: Monad m => a -> Maybe b -> ExceptT a m b
noteET l = hoistEither . note l
