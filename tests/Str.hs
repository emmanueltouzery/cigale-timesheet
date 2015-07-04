{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module Str (strT, strCrT) where

import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List
import Data.Maybe
import Control.Error
import Control.Monad (void)

import qualified Data.Text as T

quotedStr :: String -> Q Exp
quotedStr str = [| T.pack $ manageWhitespace str |]

quotedStrCr :: String -> Q Exp
quotedStrCr str = [| T.concat [T.pack $ manageWhitespace str, "\n"] |]

{-
  I first expect a carriage return, then on the next line I'll skip
  all the characters until the first actual text, and then i'll skip
  the same number of spaces for every line after.
-}
manageWhitespace :: String -> String
manageWhitespace input = fromMaybe (error "Parse error") $
    hush $ parse manageWhitespaceParser "" input

manageWhitespaceParser :: GenParser Char st String
manageWhitespaceParser = do
    eol
    -- check the number of characters until the first
    -- non-whitespace character.
    whitespace <- many $ oneOf "\t "
    let spacesCount = length whitespace
    firstLine <- many $ noneOf "\r\n"
    void eol <|> eof
    otherLines <- many (lineSkipSpaces spacesCount <|> eol)
    eof
    return $ intercalate "\n" (firstLine:otherLines)

lineSkipSpaces :: Int -> GenParser Char st String
lineSkipSpaces spacesCount = do
    count spacesCount (oneOf "\t ")
    result <- many $ noneOf "\r\n"
    void eol <|> eof
    return result

eol :: GenParser Char st String
eol = optional (string "\r") >> string "\n" >> return ""

strT :: QuasiQuoter
strT = QuasiQuoter quotedStr undefined undefined undefined

-- extra carriage return at the end
strCrT :: QuasiQuoter
strCrT = QuasiQuoter quotedStrCr undefined undefined undefined
