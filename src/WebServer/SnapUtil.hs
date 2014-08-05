{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module SnapUtil where

import Snap.Core
import qualified Data.ByteString as BS
import Control.Error
import Control.Monad.Trans
import Control.Applicative

-- rqParam returns an array in case one value is sent several times.
-- My client won't send several times, get just the first value.
getSingleParam :: BS.ByteString -> Snap (Maybe BS.ByteString)
getSingleParam pName = do
	rq <- getRequest 
	case rqParam pName rq of
		(Just (pVal:[])) -> return $ Just pVal
		_ -> return Nothing

setResponse :: Either BS.ByteString BS.ByteString -> Snap ()
setResponse (Left msg) = modifyResponse $ setResponseStatus 500 msg
setResponse (Right val) = writeBS val

setActionResponse :: EitherT BS.ByteString Snap BS.ByteString -> Snap ()
setActionResponse action = runEitherT action >>= setResponse

hParam :: BS.ByteString -> EitherT BS.ByteString Snap BS.ByteString
hParam t = do
	param <- rqParam t <$> lift getRequest
	hoistEither $ note (BS.append "Parameter missing: " t) param >>= eHead t

eHead :: BS.ByteString -> [a] -> Either BS.ByteString a
eHead _ (x:_) = Right x
eHead t _ = Left $ BS.append "Parameter empty: " t
