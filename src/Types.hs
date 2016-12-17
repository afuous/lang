module Types where

import Control.Monad.Trans.Either
import Control.Monad.State
import qualified Data.Map as Map


type Block = [Instr]

newtype Ident = Ident { unIdent :: String } deriving (Eq, Ord)

data Instr = LetInstr Ident (Maybe Expr)
           | Assignment Ident Expr
           | IfElseBlock Expr Block (Maybe Block)
           | WhileBlock Expr Block
           | OutputInstr Expr
           | InputInstr Ident

data Expr = Constant Value
          | Variable Ident
          | Operator Op Expr Expr

type Vars = [Map.Map Ident Value]

type Action a = StateT Vars (EitherT Value IO) a

data Value = LangInt Integer
           | LangStr String
           | LangBool Bool
           | LangNull -- :(

data Op = Op
  { opSymbol :: String
  , opName :: String
  , opAssoc :: Assoc
  }

data Assoc = RAssoc
           | LAssoc
           | NoAssoc
