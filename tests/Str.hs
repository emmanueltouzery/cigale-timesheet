{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Str (strT) where
 
import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List

import qualified Data.Text as T

quotedStr :: String -> Q Exp
quotedStr str = [| T.pack $ manageWhitespace str |]

{-
  I first expect a carriage return, then on the next line I'll skip
  all the characters until the first actual text, and then i'll skip
  the same number of spaces for every line after.
-}
manageWhitespace :: String -> String
manageWhitespace input =
	let
		parseResult = parse manageWhitespaceParser "" input 
	in
		case parseResult of
			Left _ -> error "Parse error"
			Right x -> x

manageWhitespaceParser :: GenParser Char st String
manageWhitespaceParser = do
	optional $ string "\r"
	string "\n"
	-- check the number of characters until the first
	-- non-whitespace character.
	whitespace <- many $ oneOf "\t "
	let spacesCount = length whitespace
	firstLine <- many $ noneOf "\r\n"
	many $ oneOf "\r\n"
	otherLines <- many $ lineSkipSpaces spacesCount
	return $ intercalate "\n" (firstLine:otherLines)

lineSkipSpaces :: Int -> GenParser Char st String
lineSkipSpaces spaces = do
	count spaces (oneOf "\t ")
	result <- many $ noneOf "\r\n"
	many $ oneOf "\r\n"
	return result

strT :: QuasiQuoter
strT = QuasiQuoter quotedStr undefined undefined undefined
