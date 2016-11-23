module Types where

import           Control.Monad.Trans.Either
import           Control.Monad.State
import qualified Data.Map as Map


type Block = [Instr]

newtype Ident = Ident { unIdent :: String } deriving (Eq, Ord)

data Instr = Assignment Ident Expr
           | IfElseBlock Expr Block (Maybe Block)
           | WhileBlock Expr Block
           | Call Expr [Expr]
           | Return Expr

data Expr = Constant Value
          | Variable Ident
          | Operator Op Expr Expr
          | FuncDef [Ident] Block
          | FuncCall Expr [Expr]

type Vars = [Map.Map Ident Value]

type Action a = StateT Vars (EitherT Value IO) a

data Value = LangInt Integer
           | LangStr String
           | LangBool Bool
           | LangFunc [Ident] Block
           | LangNull -- :(
           | BuiltInFunc ([Value] -> Action Value)

data Op = Op
  { symbol :: String
  , call :: Value -> Value -> Value
  , assoc :: Assoc
  }

data Assoc = RAssoc
           | LAssoc
           | NoAssoc
