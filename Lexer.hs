module Lexer (lexer) where

import           Data.Char (isDigit, isLetter, isSpace)
import           Data.List (find, isPrefixOf)
import qualified Data.Map as Map
import           Types

charTokens :: Map.Map Char Token
charTokens = Map.fromList [ ('(', TLeftParen)
                          , (')', TRightParen)
                          , ('=', TEquals)
                          , (';', TSemicolon)
                          ]

wordTokens :: Map.Map String Token
wordTokens = Map.fromList [ ("output", TOutput)
                          , ("input", TInput)
--                          , ("if", TIf)
                          ]

operatorTokens :: [Char]
operatorTokens = ['+', '-', '*', '/', '^']

getWordToken :: String -> Maybe String
getWordToken xs = find startsWith (Map.keys wordTokens)
  where
    startsWith str = let len = length str in str `isPrefixOf` xs
                                          && length xs > len
                                          && isSpace (xs !! len)

getLiteral :: String -> Maybe String
getLiteral [] = Nothing
getLiteral (x:xs)
    | isDigit x = Just $ case getLiteral xs of
                      Nothing -> [x]
                      Just str -> x:str
    | otherwise = Nothing

getIdent :: String -> String
getIdent [] = []
getIdent (x:xs)
    | isLetter x = x : getIdent xs
    | otherwise = []

lexer :: String -> [Token]
lexer [] = []
lexer xs@(x:xt)
    | isSpace x = lexer xt
    | Just token <- Map.lookup x charTokens = token : lexer xt
    | x `elem` operatorTokens = TOperator x : lexer xt
    | Just str <- getWordToken xs = wordTokens Map.! str : lexer (drop (length str) xs)
    | Just str <- getLiteral xs = TLiteral (Value $ read str) : lexer (drop (length str) xs)
    | otherwise = let str = getIdent xs in TIdent (Ident str) : lexer (drop (length str) xs)
