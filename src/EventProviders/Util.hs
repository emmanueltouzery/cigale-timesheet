{-# LANGUAGE OverloadedStrings, FlexibleContexts, ViewPatterns, ScopedTypeVariables #-}

module Util where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import Data.Char (digitToInt)
import Data.List (transpose)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Text as T
import Data.Time.Clock
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Network.Http.Client
import System.IO.Streams (InputStream(..))
import Network.URI
import qualified System.IO.Streams as Streams
import qualified Blaze.ByteString.Builder as Builder
import qualified System.Info as Sysinfo
import qualified OpenSSL.Session as SSL
import qualified Data.Aeson as A
import qualified Data.Attoparsec as AP
import qualified Data.Functor.Identity as DFI
import Control.Applicative
import Control.Error
import Control.Exception as Ex
import Control.Monad (liftM)
import Text.Printf (printf)

import OpenSSL (withOpenSSL)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

parseNum :: (Num a, Read a) => Int -> T.GenParser st a
parseNum digitCount = read <$> count digitCount digit

-- return the common prefix to all the files.
-- http://www.haskell.org/pipermail/beginners/2011-April/006861.html
getFilesRoot :: [String] -> String
getFilesRoot allFiles = map head (takeWhile allEqual (zipLists allFiles))

allEqual :: Eq a => [a] -> Bool
allEqual list = all (== head list) list

-- http://stackoverflow.com/a/2468379/516188
-- I added the takeWhile in case the lengths
-- of the lists don't match.
zipLists :: [[a]] -> [[a]]
zipLists list = takeWhile (\x -> length x == length list) $ transpose list

-- ability to fail with a user-provided error message,
-- independant of the nature of the parse error.
parse2 :: (T.Stream s DFI.Identity t, Show s) => T.Parsec s () a -> String -> s -> Either String a
parse2 parser errorV input = fmapL (const $ errorV ++ ": " ++ show input) $ parse parser "" input

parseMaybe :: (T.Stream s DFI.Identity t) => T.Parsec s () a -> s -> Maybe a
parseMaybe parser = hush . parse parser ""

parsecError :: (T.Stream s DFI.Identity t, Show s) => T.Parsec s () a -> String -> s -> a
parsecError parser errorText input = case parse parser "" input of
	Left a -> error $ errorText ++ show a
	Right x -> x

formatDurationSec :: NominalDiffTime -> T.Text
formatDurationSec (round -> seconds :: Int) = T.pack $ printf "%d:%02d" hours minutes
	where
		hours = seconds `div` 3600
		minutes = (seconds `mod` 3600) `div` 60

http :: B.ByteString -> B.ByteString 
		-> (Response -> InputStream B.ByteString -> IO B.ByteString) 
		-> RequestBuilder a ->  IO B.ByteString
http url contents responseProcessor requestSpec = withOpenSSL $ do
	c <- establishConnection url
	q <- buildRequest requestSpec
	sendRequest c q $ Streams.write (Just $ Builder.fromByteString contents)
	result <- receiveResponse c responseProcessor
	closeConnection c
	return result

-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary monads.
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- 99% when using try, i want to catch all, so SomeException.
-- save myself the ScopedTypeVariables and typing there.
tryS :: IO a -> IO (Either SomeException a)
tryS = Ex.try
