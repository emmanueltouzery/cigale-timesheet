module SnapUtil where

import Snap.Core
import qualified Data.ByteString as BS

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
