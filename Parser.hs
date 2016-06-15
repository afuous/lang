module Parser (parseBlock) where

import           Control.Monad (msum)
import           Data.List (elemIndex)
import           Types

parseBlock :: [Token] -> Block
parseBlock = map parseInstruction . splitInstructions

--subBlocks :: [Token] -> [[Token]]
splitInstructions :: [Token] -> [[Token]]
splitInstructions tokens = case TokenSemicolon `elemIndex` tokens of
    Nothing -> []
    Just n -> take n tokens : splitInstructions (drop (n + 1) tokens)

parseInstruction :: [Token] -> Instruction
parseInstruction (TokenOutput:xs) = Output (parseExpression xs)
parseInstruction [TokenInput, (TokenIdentifier ident)] = Input ident
parseInstruction ((TokenIdentifier ident):TokenEquals:xs) = Assignment ident (parseExpression xs)
parseInstruction _ = error "syntax error"

operators :: [[Char]]
operators = [ ['+', '-']
            , ['*', '/']
            , ['/']
            ]

parseExpression :: [Token] -> Expression
parseExpression [] = error "empty expression"
parseExpression [TokenLiteral value] = Constant value
parseExpression [TokenIdentifier ident] = Variable ident
parseExpression xs =
    let rev = reverse xs
    in case findOperator rev of
        Nothing -> if head xs == TokenLeftParen && last xs == TokenRightParen
            then parseExpression $ init $ tail xs
            else error "invalid expression"
        Just pos -> let right = reverse $ take pos rev
                        left = reverse $ drop (pos + 1) rev
                        (TokenOperator char) = rev !! pos
                    in Operator char (parseExpression left) (parseExpression right)
  where

    findOperator :: [Token] -> Maybe Int
    findOperator rev = msum $ map (walk rev 0) operators

    walk :: [Token] -> Int -> [Char] -> Maybe Int
    walk [] _ _ = Nothing
    walk _ n _ | n < 0 = error "mismatched parentheses"
    walk (TokenRightParen:xs) n ops = fmap (+1) $ walk xs (n + 1) ops
    walk (TokenLeftParen:xs) n ops = fmap (+1) $ walk xs (n - 1) ops
    walk ((TokenOperator op):xs) 0 ops | op `elem` ops = Just 0
    walk (x:xs) n ops = fmap (+1) $ walk xs n ops
