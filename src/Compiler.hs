{-# LANGUAGE QuasiQuotes #-}

module Compiler (compile) where

import Data.String.QQ
import Types

beginning :: String
beginning = [s|
#include <stdio.h>
#include <stdlib.h>

typedef enum { LANG_INT } lang_type;

typedef struct lang_value {
  lang_type type;
  union {
    int int_value;
  } value;
} lang_value;

lang_value make_lang_value(int n) {
  lang_value val;
  val.type = LANG_INT;
  val.value.int_value = n;
  return val;
}

int get_input(void) {
  int result = 0;
  char c;
  while ((c = getchar()) != '\n') {
    if ('0' <= c && c <= '9') {
      result = result * 10 + c - '0';
    }
  }
  return result;
}

int main(void) {
|]

ending :: String
ending = [s|
return 0;
}
|]

compile :: Block -> String
compile block = beginning ++ compileBlock block ++ ending

compileBlock :: Block -> String
compileBlock = foldr (++) "" . map ((++ "\n") . compileInstr)

compileInstr :: Instr -> String
compileInstr (LetInstr ident mExpr) =
  "lang_value " ++ unIdent ident ++ case mExpr of
    Nothing -> ";"
    Just expr -> " = " ++ compileExpr expr ++ ";"
compileInstr (Assignment ident expr) =
  unIdent ident ++ " = " ++ compileExpr expr ++ ";"
compileInstr (IfElseBlock cond whenTrue mWhenFalse) =
  "if (" ++ compileExpr cond ++ ".value.int_value) {\n" ++ compileBlock whenTrue ++ "}" ++
    case mWhenFalse of
      Just whenFalse -> "else {\n" ++ compileBlock whenFalse ++ "}"
      Nothing -> ""
compileInstr (WhileBlock cond block) =
  "while (" ++ compileExpr cond ++ ".value.int_value) {\n" ++ compileBlock block ++ "}"
compileInstr (OutputInstr expr) =
  "printf(\"%d\\n\", " ++ compileExpr expr ++ ".value.int_value);"
compileInstr (InputInstr ident) =
  unIdent ident ++ " = make_lang_value(get_input());"

compileExpr :: Expr -> String
compileExpr (Constant (LangInt n)) = "make_lang_value(" ++ show n ++ ")"
compileExpr (Variable ident) = unIdent ident
compileExpr (Operator op expr1 expr2) =
  "make_lang_value(" ++ compileExpr expr1 ++ ".value.int_value " ++ symbol op ++ " " ++ compileExpr expr2 ++ ".value.int_value)"
