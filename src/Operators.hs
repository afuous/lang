{-# LANGUAGE QuasiQuotes #-}

module Operators
  ( operators
  , cOperators
  ) where

import Data.String.QQ
import Types
import Text.Parsec


operators =
  [ [ Op "&&" "and" LAssoc
    , Op "||" "or" LAssoc
    ]
  , [ Op "==" "equals" NoAssoc
    , Op "!=" "notequals" NoAssoc
    , Op "<" "lessthan" NoAssoc
    , Op ">" "greaterthan" NoAssoc
    , Op "<=" "lessthanequals" NoAssoc
    , Op ">=" "greaterthanequals" NoAssoc
    ]
  , [ Op "+" "add" LAssoc
    , Op "-" "subtract" LAssoc
    ]
  , [ Op "*" "multiply" LAssoc
    , Op "/" "divide" LAssoc
    , Op "%" "modulo" LAssoc ]
  , [ Op "^" "exponent" RAssoc
    ]
  ]

cOperators :: String
cOperators = [s|

lang_value operator_and(lang_value a, lang_value b) {
  require(a.type == LANG_BOOL && b.type == LANG_BOOL);
  return make_lang_bool(a.value.bool_value && b.value.bool_value);
}

lang_value operator_or(lang_value a, lang_value b) {
  require(a.type == LANG_BOOL && b.type == LANG_BOOL);
  return make_lang_bool(a.value.bool_value || b.value.bool_value);
}

lang_value operator_equals(lang_value a, lang_value b) {
  require(a.type == b.type);
  switch (a.type) {
    case LANG_INT:
      return make_lang_bool(a.value.int_value == b.value.int_value);
    case LANG_BOOL:
      return make_lang_bool(a.value.bool_value == b.value.bool_value);
    default:
      require(0);
  }
}

lang_value operator_notequals(lang_value a, lang_value b) {
  require(a.type == b.type);
  switch (a.type) {
    case LANG_INT:
      return make_lang_bool(a.value.int_value != b.value.int_value);
    case LANG_BOOL:
      return make_lang_bool(a.value.bool_value != b.value.bool_value);
    default:
      require(0);
  }
}

lang_value operator_lessthan(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_bool(a.value.int_value < b.value.int_value);
}

lang_value operator_greaterthan(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_bool(a.value.int_value > b.value.int_value);
}

lang_value operator_lessthanequals(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_bool(a.value.int_value <= b.value.int_value);
}

lang_value operator_greaterthanequals(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_bool(a.value.int_value >= b.value.int_value);
}

lang_value operator_add(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_int(a.value.int_value + b.value.int_value);
}

lang_value operator_subtract(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_int(a.value.int_value - b.value.int_value);
}

lang_value operator_multiply(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_int(a.value.int_value * b.value.int_value);
}

lang_value operator_divide(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_int(a.value.int_value / b.value.int_value);
}

lang_value operator_modulo(lang_value a, lang_value b) {
  require(a.type == LANG_INT && b.type == LANG_INT);
  return make_lang_int(a.value.int_value % b.value.int_value);
}

|]
