module Parser (parseBlock) where

import           Data.List (elemIndex)
import           Types

parseBlock :: [Token] -> Block
parseBlock = map parseInstruction . splitInstructions

--subBlocks :: [Token] -> [[Token]]
splitInstructions :: [Token] -> [[Token]]
splitInstructions tokens = case TokenSemicolon `elemIndex` tokens of
    Nothing -> [] -- change to [] for requiring trailing semicolon
    Just n -> take n tokens : splitInstructions (drop (n + 1) tokens)

parseInstruction :: [Token] -> Instruction
parseInstruction (TokenOutput:xs) = Output (parseExpression xs)
parseInstruction [TokenInput, (TokenIdentifier ident)] = Input ident
parseInstruction ((TokenIdentifier ident):TokenEquals:xs) = Assignment ident (parseExpression xs)
parseInstruction _ = error "syntax error"

parseExpression :: [Token] -> Expression
parseExpression [] = error "empty expression"
parseExpression [TokenLiteral value] = Constant value
parseExpression [TokenIdentifier ident] = Variable ident
parseExpression xs =
    let rev = reverse xs
    in case walk rev 0 of
        Nothing -> if head xs == TokenLeftParen && last xs == TokenRightParen
            then parseExpression $ init $ tail xs
            else error "invalid expression"
        Just pos -> let right = reverse $ take pos rev
                        left = reverse $ drop (pos + 1) rev
                        (TokenOperator char) = rev !! pos
                    in Operator char (parseExpression left) (parseExpression right)
  where
    walk [] _ = Nothing
    walk _ n | n < 0 = error "mismatched parentheses"
    walk (TokenRightParen:xs) n = fmap (+1) $ walk xs (n + 1)
    walk (TokenLeftParen:xs) n = fmap (+1) $ walk xs (n - 1)
    walk ((TokenOperator _):xs) 0 = Just 0
    walk (x:xs) n = fmap (+1) $ walk xs n
