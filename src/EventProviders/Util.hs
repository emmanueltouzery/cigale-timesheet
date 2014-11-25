{-# LANGUAGE OverloadedStrings, FlexibleContexts, ViewPatterns, ScopedTypeVariables, LambdaCase #-}

module Util where

import qualified Data.ByteString as B
import Data.Char (digitToInt)
import Data.List (transpose)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Text as T
import Data.Time.Clock
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Network.Http.Client as S
import System.IO.Streams (InputStream(..))
import qualified System.IO.Streams as Streams
import qualified Blaze.ByteString.Builder as Builder
import qualified OpenSSL.Session as SSL
import qualified Data.Aeson as A
import qualified Data.Functor.Identity as DFI
import Control.Applicative
import Control.Error
import Control.Exception as Ex
import Control.Monad (liftM)
import Text.Printf (printf)
import System.Process
import qualified Data.Text.IO as IO
import System.Exit
import Control.Monad.Trans

import OpenSSL (withOpenSSL)

parseNum :: (Num a, Read a) => Int -> T.GenParser st a
parseNum digitCount = read <$> count digitCount digit

runProcess :: String -> String -> [String] -> EitherT String IO T.Text
runProcess program runningFolder parameters = do
	(inh, Just outh, errh, pid) <- lift $ createProcess (proc program parameters)
		{ std_out = CreatePipe, cwd = Just runningFolder }
	output <- lift (IO.hGetContents outh)
	lift (waitForProcess pid) >>= \case
		-- TODO collect error output on failure.
		ExitFailure x -> hoistEither $ Left (printf "%s failed with exit code %d" program x :: String)
		ExitSuccess -> return output

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

-- ############# THIS MUST GO.
parsecError :: (T.Stream s DFI.Identity t, Show s) => T.Parsec s () a -> String -> s -> a
parsecError parser errorText input = case parse parser "" input of
	Left a -> error $ errorText ++ show a
	Right x -> x

formatDurationSec :: NominalDiffTime -> T.Text
formatDurationSec (round -> seconds :: Int) = T.pack $ printf "%d:%02d" hours minutes
	where
		hours = seconds `div` 3600
		minutes = (seconds `mod` 3600) `div` 60

requestDefaults :: RequestBuilder ()
requestDefaults = return ()

http :: Method -> B.ByteString -> B.ByteString
		-> (Response -> InputStream B.ByteString -> IO B.ByteString) 
		-> RequestBuilder a ->  IO B.ByteString
http method url contents responseProcessor requestSpec = withOpenSSL $ do
	c <- establishConnection url
	q <- buildRequest $ S.http method url >> requestSpec
	sendRequest c q $ Streams.write (Just $ Builder.fromByteString contents)
	result <- receiveResponse c responseProcessor
	closeConnection c
	return result

-- 99% when using try, i want to catch all, so SomeException.
-- save myself the ScopedTypeVariables and typing there.
tryS :: IO a -> IO (Either SomeException a)
tryS = Ex.try
