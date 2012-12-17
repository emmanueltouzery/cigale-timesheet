module Util where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Foreign.ForeignPtr
import           Foreign.Ptr

safePromise :: Either a (b,c) -> b
safePromise (Right (v,_)) = v

-- http://stackoverflow.com/questions/7815402/convert-a-lazy-bytestring-to-a-strict-bytestring/13632110#comment19162473_13632110
-- to replace to toStrict when upgrading to a recent enough haskell...
toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks
