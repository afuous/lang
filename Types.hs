module Types where

import           Control.Monad.State
import qualified Data.Map as Map

-- types for lexing

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


-- types for parsing

type Block = [Instruction]

newtype Identifier = Identifier { unIdentifier :: String } deriving (Show, Eq, Ord)

newtype Value = Value { unValue :: Integer } deriving (Show, Eq)

data Instruction = Assignment Identifier Expression
                 | Output Expression
                 | Input Identifier
                 deriving Show

data Expression = Constant Value
                | Variable Identifier
                | Operator Char Expression Expression
                deriving Show


-- types for execution

type Variables = Map.Map Identifier Value

type Action = StateT Variables IO ()
