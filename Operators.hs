{-# LANGUAGE FlexibleInstances #-}

module Operators where

import           Text.Parsec


data Op = Op
  { symbol :: String
  , call :: Integer -> Integer -> Integer
  , assoc :: Assoc
  } deriving Show

instance Show (Integer -> Integer -> Integer) where
  show _ = "function"

data Assoc = RAssoc | LAssoc deriving Show

operators =
  [ [ Op "+" (+) LAssoc
    , Op "-" (-) LAssoc ]
  , [ Op "*" (*) LAssoc
    , Op "/" div LAssoc
    , Op "%" mod LAssoc ]
  , [ Op "^" (^) RAssoc ]
  ]
