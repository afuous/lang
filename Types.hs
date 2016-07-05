module Types where

import           Control.Monad.State
import qualified Data.Map as Map


-- types for parsing

type Block = [Instr]

newtype Ident = Ident { unIdent :: String } deriving (Show, Eq, Ord)

newtype Value = Value { unValue :: Integer } deriving (Show, Eq)

data Instr = Assignment Ident Expr
           | Output Expr
           | Input Ident
           | IfBlock Expr Block
           | WhileBlock Expr Block
           deriving Show

data Expr = Constant Value
          | Variable Ident
          | Operator Char Expr Expr
          deriving Show


-- types for execution

type Vars = [Map.Map Ident Value]

type Action a = StateT Vars IO a

