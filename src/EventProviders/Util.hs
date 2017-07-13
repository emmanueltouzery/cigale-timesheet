{-# LANGUAGE OverloadedStrings, FlexibleContexts, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, QuasiQuotes #-}

module Util where

import qualified Data.ByteString as B
import Data.List (transpose)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock
import Data.Char
import Data.Aeson.TH
import Data.Aeson.Types hiding (parse)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Network.Http.Client as S
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Functor.Identity as DFI
import Control.Error
import Control.Exception as Ex
import Text.Printf.TH
import System.Process
import qualified Data.Text.IO as IO
import System.Exit
import Control.Monad.Trans
import Control.Monad

import OpenSSL (withOpenSSL)

parseNum :: (Num a, Read a) => Int -> T.GenParser st a
parseNum digitCount = read <$> count digitCount digit

runProcess :: String -> String -> [String] -> ExceptT String IO Text
runProcess program runningFolder parameters = do
    (_, Just outh, _, pid) <- lift $ createProcess (proc program parameters)
        { std_out = CreatePipe, cwd = Just runningFolder }
    output <- lift (IO.hGetContents outh)
    lift (waitForProcess pid) >>= \case
        -- TODO collect error output on failure.
        ExitFailure x -> hoistEither (Left $ [s|%s failed with exit code %d|] program x)
        ExitSuccess -> return output

-- return the common prefix to all the files.
-- http://www.haskell.org/pipermail/beginners/2011-April/006861.html
getFilesRoot :: [String] -> String
getFilesRoot allFiles = head <$> takeWhile allEqual (zipLists allFiles)

allEqual :: Eq a => [a] -> Bool
allEqual []   = True
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

eol :: T.GenParser st ()
eol = void $ optional (char '\r') *> char '\n'

formatDurationSec :: NominalDiffTime -> Text
formatDurationSec (round -> seconds :: Int) = [st|%d:%02d|] hours minutes
    where
        hours = seconds `div` 3600
        minutes = (seconds `mod` 3600) `div` 60

requestDefaults :: RequestBuilder ()
requestDefaults = return ()

linksForceNewWindow :: Text -> Text
linksForceNewWindow = T.replace "<a href=" "<a target='_blank' href="

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

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = return Nothing
findM f (x:xs) = f x >>= bool (findM f xs) (return $ Just x)

camelCaseJsonDecoder name = defaultOptions
  {
    fieldLabelModifier = fmap toLower . camelTo2 '_' . drop (T.length name)
  }
