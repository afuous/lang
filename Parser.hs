module Parser (parseBlock) where

import           Control.Monad (msum)
import           Data.List (elemIndex)
import           Data.Maybe (fromJust)
import           Types

parseBlock :: [Token] -> Block
parseBlock [] = []
parseBlock (TOutput:xs) =
    let count = findToken TSemicolon xs
    in (Output $ parseExpr $ take count xs) : parseBlock (drop (count + 1) xs)
parseBlock (TInput:(TIdent ident):xs) =
    (Input ident) : parseBlock (drop 1 xs)
parseBlock ((TIdent ident):TEquals:xs) =
    let count = findToken TSemicolon xs
    in (Assignment ident $ parseExpr $ take count xs) : parseBlock (drop (count + 1) xs)
parseBlock (TIf:xs) =
    let count1 = findToken TLeftCurl xs
        count2 = findMatchingCurl $ drop (count1 + 1) xs
    in (IfBlock (parseExpr $ take count1 xs) (parseBlock $ take count2 $ drop (count1 + 1) xs)) : parseBlock (drop (count1 + count2 + 2) xs)
parseBlock (TWhile:xs) =
    let count1 = findToken TLeftCurl xs
        count2 = findMatchingCurl $ drop (count1 + 1) xs
    in (WhileBlock (parseExpr $ take count1 xs) (parseBlock $ take count2 $ drop (count1 + 1) xs)) : parseBlock (drop (count1 + count2 + 2) xs)
parseBlock _ = error "syntax error"

findToken :: Token -> [Token] -> Int
findToken token tokens = fromJust $ token `elemIndex` tokens

findMatchingCurl :: [Token] -> Int
findMatchingCurl xs = walk xs 1
  where
    walk :: [Token] -> Int -> Int
    walk [] _ = error "mismatched brackets"
    walk (TLeftCurl:xs) n = 1 + walk xs (n + 1)
    walk (TRightCurl:_) 1 = 0 -- exclusive of last }
    walk (TRightCurl:xs) n = 1 + walk xs (n - 1)
    walk (_:xs) n = 1 + walk xs n

operators :: [[Char]]
operators = [ ['+', '-']
            , ['*', '/', '%']
            , ['/']
            ]

parseExpr :: [Token] -> Expr
parseExpr [] = error "empty expression"
parseExpr [TLiteral value] = Constant value
parseExpr [TIdent ident] = Variable ident
parseExpr xs =
    let rev = reverse xs
    in case findOperator rev of
        Nothing -> if head xs == TLeftParen && last xs == TRightParen
            then parseExpr $ init $ tail xs
            else error "invalid expression"
        Just pos -> let right = reverse $ take pos rev
                        left = reverse $ drop (pos + 1) rev
                        (TOperator char) = rev !! pos
                    in Operator char (parseExpr left) (parseExpr right)
  where

    findOperator :: [Token] -> Maybe Int
    findOperator rev = msum $ map (walk rev 0) operators

    walk :: [Token] -> Int -> [Char] -> Maybe Int
    walk [] _ _ = Nothing
    walk _ n _ | n < 0 = error "mismatched parentheses"
    walk (TRightParen:xs) n ops = fmap (+1) $ walk xs (n + 1) ops
    walk (TLeftParen:xs) n ops = fmap (+1) $ walk xs (n - 1) ops
    walk ((TOperator op):xs) 0 ops | op `elem` ops = Just 0
    walk (x:xs) n ops = fmap (+1) $ walk xs n ops
