{-# LANGUAGE OverloadedStrings #-}

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

import OpenSSL (withOpenSSL)

safePromise :: Either a (b,c) -> b
safePromise (Right (v,_)) = v
safePromise _ = error "safePromise got Left"

-- http://stackoverflow.com/questions/7815402/convert-a-lazy-bytestring-to-a-strict-bytestring/13632110#comment19162473_13632110
-- to replace to toStrict when upgrading to a recent enough haskell...
toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

parsedToInt :: String -> Int
parsedToInt digits = foldl ((+).(*10)) 0 (map digitToInt digits)

parsedToInteger :: String -> Integer
parsedToInteger = fromIntegral . parsedToInt

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

-- parsecParse :: T.Text -> (T.GenParser st T.Text) -> IO T.Text
parsecParse parser input = do
	case parse parser "" input of
		Left pe -> do
			putStrLn $ "parse error: " ++ displayErrors pe
			return input
		Right result -> return result

displayErrors :: ParseError -> String
displayErrors pe = concat $ fmap messageString' (errorMessages pe) ++
		["position: ", displayErrorPos $ errorPos pe]
	where
		displayErrorPos pos = (show $ sourceLine pos) ++ ":" ++ (show $ sourceColumn pos)

messageString' :: Message -> String
messageString' msg
    = case msg of SysUnExpect s -> "sys unexpect " ++ s
                  UnExpect s    -> "unexpect " ++ s
                  Expect s      -> "expect " ++ s
                  Message s     -> "message" ++ s  

formatDurationSec :: NominalDiffTime -> T.Text
formatDurationSec seconds = T.concat [T.pack hours, ":", 
				T.justifyRight 2 '0' $ T.pack minutes]
	where
		secondsI = round seconds :: Int
		hours = show $ secondsI `div` 3600
		minutes = show $ (secondsI `mod` 3600) `div` 60

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