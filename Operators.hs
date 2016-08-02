module Operators where

import           Types
import           Text.Parsec


operators =
  [ [ Op "&&" langAnd LAssoc
    , Op "||" langOr LAssoc ]
  , [ Op "==" langEq NoAssoc
    , Op "!=" langNotEq NoAssoc
    , Op "<" langLt NoAssoc
    , Op ">" langGt NoAssoc
    , Op "<=" langLtEq NoAssoc
    , Op ">=" langGtEq NoAssoc ]
  , [ Op "+" langAdd LAssoc
    , Op "-" langSub LAssoc ]
  , [ Op "*" langMul LAssoc
    , Op "/" langDiv LAssoc
    , Op "%" langMod LAssoc ]
  , [ Op "^" langExp RAssoc ]
  ]

langAnd (LangBool a) (LangBool b) = LangBool (a && b)
langOr (LangBool a) (LangBool b) = LangBool (a || b)
langEq (LangBool a) (LangBool b) = LangBool (a == b)
langEq (LangInt a) (LangInt b) = LangBool (a == b)
langEq (LangStr a) (LangStr b) = LangBool (a == b)
langNotEq (LangBool a) (LangBool b) = LangBool (a /= b)
langNotEq (LangInt a) (LangInt b) = LangBool (a /= b)
langNotEq (LangStr a) (LangStr b) = LangBool (a /= b)
langLt (LangInt a) (LangInt b) = LangBool (a < b)
langGt (LangInt a) (LangInt b) = LangBool (a > b)
langLtEq (LangInt a) (LangInt b) = LangBool (a <= b)
langGtEq (LangInt a) (LangInt b) = LangBool (a >= b)
langAdd (LangInt a) (LangInt b) = LangInt (a + b)
langSub (LangInt a) (LangInt b) = LangInt (a - b)
langMul (LangInt a) (LangInt b) = LangInt (a * b)
langDiv (LangInt a) (LangInt b) = LangInt (a `div` b)
langMod (LangInt a) (LangInt b) = LangInt (a `mod` b)
langExp (LangInt a) (LangInt b) = LangInt (a ^ b)
