{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Control.Monad.Trans.Either
import           Control.Monad.State
import qualified Data.Map as Map


type Block = [Instr]

newtype Ident = Ident { unIdent :: String } deriving (Show, Eq, Ord)

data Instr = Assignment Ident Expr
           | Output Expr
           | Input Ident
           | IfElseBlock Expr Block (Maybe Block)
           | WhileBlock Expr Block
           | Call Expr [Expr]
           | Return Expr
           deriving Show

data Expr = Constant Value
          | Variable Ident
          | Operator Op Expr Expr
          | FuncDef [Ident] Block
          | FuncCall Expr [Expr]
          deriving Show

type Vars = [Map.Map Ident Value]

type Action a = StateT Vars (EitherT Value IO) a

data Value = LangInt Integer
           | LangStr String
           | LangBool Bool
           | LangFunc [Ident] Block
           deriving Show

data Op = Op
  { symbol :: String
  , call :: Value -> Value -> Value
  , assoc :: Assoc
  } deriving Show

instance Show (Value -> Value -> Value) where
  show _ = "function"

data Assoc = RAssoc
           | LAssoc
           | NoAssoc
           deriving Show
