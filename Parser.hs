module Parser (parseBlock) where

import           Control.Monad (msum)
import           Data.List (elemIndex)
import           Types

parseBlock :: [Token] -> Block
parseBlock = map parseInstr . splitInstrs

--subBlocks :: [Token] -> [[Token]]
splitInstrs :: [Token] -> [[Token]]
splitInstrs tokens = case TSemicolon `elemIndex` tokens of
    Nothing -> []
    Just n -> take n tokens : splitInstrs (drop (n + 1) tokens)

parseInstr :: [Token] -> Instr
parseInstr (TOutput:xs) = Output (parseExpr xs)
parseInstr [TInput, (TIdent ident)] = Input ident
parseInstr ((TIdent ident):TEquals:xs) = Assignment ident (parseExpr xs)
parseInstr _ = error "syntax error"

operators :: [[Char]]
operators = [ ['+', '-']
            , ['*', '/']
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
