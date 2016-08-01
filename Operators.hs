module Operators where

import           Types
import           Text.Parsec


operators =
  [ [ Op "+" langAdd LAssoc
    , Op "-" langSub LAssoc ]
  , [ Op "*" langMul LAssoc
    , Op "/" langDiv LAssoc
    , Op "%" langMod LAssoc ]
  , [ Op "^" langExp RAssoc ]
  ]

langAdd (LangInt a) (LangInt b) = LangInt (a + b)
langSub (LangInt a) (LangInt b) = LangInt (a - b)
langMul (LangInt a) (LangInt b) = LangInt (a * b)
langDiv (LangInt a) (LangInt b) = LangInt (a `div` b)
langMod (LangInt a) (LangInt b) = LangInt (a `mod` b)
langExp (LangInt a) (LangInt b) = LangInt (a ^ b)
