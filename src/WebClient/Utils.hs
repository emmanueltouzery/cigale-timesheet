{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Prelude
import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import JQuery hiding (on)

bootstrapModal :: JQuery -> Fay ()
bootstrapModal = ffi "%1.modal('show')"

bootstrapModalHide :: JQuery -> Fay ()
bootstrapModalHide = ffi "%1.modal('hide')"

data JValue

ajxPut :: Text -> Automatic a -> Fay () -> Fay () -> Fay ()
ajxPut = ffi "jQuery.ajax({type:'PUT', url: %1, data: JSON.stringify(%2)}).success(%4).fail(%3)"

ajxPost :: Text -> Automatic a -> Fay () -> Fay () -> Fay ()
ajxPost = ffi "jQuery.ajax({type:'POST', url: %1, data: JSON.stringify(%2)}).success(%4).fail(%3)"

ajxDelete :: Text -> Fay () -> Fay () -> Fay ()
ajxDelete = ffi "jQuery.ajax({type:'DELETE', url: %1}).success(%3).fail(%2)"

ajxGet :: Text -> Fay () -> (Automatic a -> Fay ()) -> Fay ()
ajxGet = ffi "jQuery.ajax({type:'GET', url:%1}).success(%3).fail(%2)"

jqParam :: Text -> JValue
--jqParam = ffi "jQuery.param(%1)"
jqParam = ffi "encodeURIComponent(%1)"

first :: JQuery -> JQuery
first = ffi "%1[0]"

loadCb :: Text -> Text -> Fay () -> Fay ()
loadCb = ffi "$(%1).load(%2, %3)"

jsLength :: JQuery -> Fay Int
jsLength = ffi "%1.length"

splitOn :: Text -> Text -> [Text]
splitOn = ffi "%2.split(%1)"

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = ffi "[%2.substring(0, %2.lastIndexOf(%1)+1), %2.substring(%2.lastIndexOf(%1)+1)]"

last :: Text -> Char
last = ffi "%1.slice(-1)"

toLower :: Text -> Text
toLower = ffi "%1.toLowerCase()"

-- no typeclasses in fay...
Right m >>> k = k
Left e >>> _ = Left e
infixr 1 >>>

isRight (Right _) = True
isRight _ = False

-- http://stackoverflow.com/questions/18521821
strComp :: String -> String -> Ordering
strComp (_:_) [] = GT
strComp [] (_:_) = LT
strComp [] [] = EQ
strComp (x:xs) (y:ys)
    | x < y = LT
    | x > y = GT
    | otherwise = strComp xs ys

(<$>) :: (a -> b) -> Fay a -> Fay b
f <$> a = do
    x <- a
    return $ f x

textComp :: Text -> Text -> Ordering
textComp = strComp `on` T.unpack

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- From Control.Monad
-- The Control.Applicative stuff
-- is cooler, but it's too much
-- to stuff here.

-- | Promote a function to a monad.
--liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- >    liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- >    liftM2 (+) (Just 1) Nothing = Nothing
--
--liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
--liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
---liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
--liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b ->  Maybe c
liftMaybe2 f (Just a) (Just b) = Just $ f a b
liftMaybe2 _ _ _                    = Nothing

liftMaybe :: (a -> b) -> Maybe a -> Maybe b
liftMaybe f (Just a) = Just $ f a
liftMaybe _ _                     = Nothing

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: got Nothing"

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
maybeFay :: b -> (a -> Fay b) -> Maybe a -> Fay b
maybeFay n _ Nothing  = return n
maybeFay _ f (Just x) = f x

--foldM             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []      =  return a
foldM f a (x:xs)  =  f a x >>= \fax -> foldM f fax xs

-- | Like 'foldM', but discards the result.
--foldM_            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs     = foldM f a xs >> return ()

-- from Data.Functions
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

-- from Data.List
inits    :: [a] -> [[a]]
inits xs =  [] : case xs of
    []      -> []
    x : xs' -> map (x :) (inits xs')

-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary monads.
concatMapM        :: (a -> Fay [b]) -> [a] -> Fay [b]
concatMapM f xs   =  liftM concat (mapM f xs)
