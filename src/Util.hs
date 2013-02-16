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

parsedToInt :: [Char] -> Int
parsedToInt digits = foldl ((+).(*10)) 0 (map digitToInt digits)

parsedToInteger :: [Char] -> Integer
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
