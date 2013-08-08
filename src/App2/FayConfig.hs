{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
import FFI
import JQuery

data ConfigDesc {
	contents :: String -- TODO deserialize to proper data
}
-- 
-- data ConfigVal {
-- 	contents :: String -- TODO deserialize to proper data
-- }
-- 
-- data ConfigInfo {
-- 	desc :: ConfigDesc,
-- 	contents :: ConfigVal
-- }

-- definitely check this!
--https://github.com/faylang/fay/blob/master/examples/Cont.hs

-- check this for state monad:
-- https://github.com/faylang/fay/blob/master/tests/Monad2.hs

-- problem is that i want both calls to START at the same time
-- and run in parrallel...
main :: Fay ()
main = ready $ myajax2 "/configVal" "/configdesc" $ \val desc -> do
		putStrLn "both AJAX returned"
		putStrLn val
		putStrLn desc

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: String -> String -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)) .then(%3)"
