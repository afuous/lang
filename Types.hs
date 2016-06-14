module Types where

-- types for parsing

data Token = TokenLeftParen
           | TokenRightParen
           | TokenEquals
           | TokenSemicolon
           | TokenOutput
           | TokenInput
           | TokenIf
           | TokenOperator Char
           | TokenLiteral Value
           | TokenIdentifier Identifier deriving Show


-- types for execution

type Block = [Instruction]

newtype Identifier = Identifier { unIdentifier :: String } deriving Show

newtype Value = Value { unValue :: Integer } deriving Show

data Instruction = Assignment Identifier Expression
                 | Output Expression
                 | Input Identifier

data Expression = Constant Value
                | Variable Identifier
                | Operator (Value -> Value) Expression Expression

