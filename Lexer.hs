module Lexer (lexer) where

import           Data.Char (isDigit, isLetter, isSpace)
import           Data.List (find, isPrefixOf)
import qualified Data.Map as Map
import           Types

charTokens :: Map.Map Char Token
charTokens = Map.fromList [ ('(', TokenLeftParen)
                          , (')', TokenRightParen)
                          , ('=', TokenEquals)
                          , (';', TokenSemicolon)
                          ]

wordTokens :: Map.Map String Token
wordTokens = Map.fromList [ ("output", TokenOutput)
                          , ("input", TokenInput)
--                          , ("if", TokenIf)
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
getLiteral (x:xs)
    | isDigit x = Just $ case getLiteral xs of
                      Nothing -> [x]
                      Just str -> x:str
    | otherwise = Nothing

getIdentifier :: String -> String
getIdentifier [] = []
getIdentifier (x:xs)
    | isLetter x = x : getIdentifier xs
    | otherwise = []

lexer :: String -> [Token]
lexer [] = []
lexer xs@(x:xt)
    | isSpace x = lexer xt
    | Just token <- Map.lookup x charTokens = token : lexer xt
    | x `elem` operatorTokens = TokenOperator x : lexer xt
    | Just str <- getWordToken xs = wordTokens Map.! str : lexer (drop (length str) xs)
    | Just str <- getLiteral xs = TokenLiteral (Value $ read str) : lexer (drop (length str) xs)
    | otherwise = let str = getIdentifier xs in TokenIdentifier (Identifier str) : lexer (drop (length str) xs)
