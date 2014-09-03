{-# LANGUAGE OverloadedStrings #-}
module SnapUtil where

import Snap.Core
import qualified Data.ByteString as BS
import Control.Error
import Control.Monad.Trans

setResponse :: Either BS.ByteString BS.ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val

setActionResponse :: EitherT BS.ByteString Snap BS.ByteString -> Snap ()
setActionResponse action = runEitherT action >>= setResponse

hParam :: BS.ByteString -> EitherT BS.ByteString Snap BS.ByteString
hParam t = lift (getParam t) >>= hoistEither . note (BS.append "Parameter missing: " t)

noteET :: a -> Maybe b -> EitherT a Snap b
noteET l = hoistEither . note l
