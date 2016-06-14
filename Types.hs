module Types where

-- types for parsing

data Token = TokenLeftParen
           | TokenRightParen
           | TokenEquals
           | TokenSemicolon
           | TokenOutput
           | TokenInput
--           | TokenIf
           | TokenOperator Char
           | TokenLiteral Value
           | TokenIdentifier Identifier 
           deriving (Show, Eq)


-- types for execution

type Block = [Instruction]

newtype Identifier = Identifier { unIdentifier :: String } deriving (Show, Eq)

newtype Value = Value { unValue :: Integer } deriving (Show, Eq)

data Instruction = Assignment Identifier Expression
                 | Output Expression
                 | Input Identifier
                 deriving Show

data Expression = Constant Value
                | Variable Identifier
                | Operator Char Expression Expression
                deriving Show

