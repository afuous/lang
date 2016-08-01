{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Control.Monad.State
import qualified Data.Map as Map


type Block = [Instr]

newtype Ident = Ident { unIdent :: String } deriving (Show, Eq, Ord)

data Instr = Assignment Ident Expr
           | Output Expr
           | Input Ident
           | IfElseBlock Expr Block (Maybe Block)
           | WhileBlock Expr Block
           deriving Show

data Expr = Constant Value
          | Variable Ident
          | Operator Op Expr Expr
          deriving Show

type Vars = [Map.Map Ident Value]

type Action a = StateT Vars IO a

data Value = LangInt Integer
           | LangStr String
           deriving (Show, Eq)

data Op = Op
  { symbol :: String
  , call :: Value -> Value -> Value
  , assoc :: Assoc
  } deriving Show

instance Show (Value -> Value -> Value) where
  show _ = "function"

data Assoc = RAssoc | LAssoc deriving Show

