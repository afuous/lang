module Parser (parseCode) where

import           Control.Monad (void)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String (Parser)
import           Types

parseCode :: String -> Either ParseError Block
parseCode code = parse (block <* eof) "syntax error" code

block :: Parser Block
block = many $   inputInstr
             <|> outputInstr
             <|> assignmentInstr

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t "

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

literal :: Parser Value
literal = lexeme $ Value <$> read <$> many1 digit

identifier :: Parser Ident
identifier = lexeme $ Ident <$> many1 letter

input, output, semicolon, equals :: Parser ()
input = void $ lexeme $ string "input"
output = void $ lexeme $ string "output"
semicolon = void $ lexeme $ char ';'
equals = void $ lexeme $ char '='

inputInstr :: Parser Instr
inputInstr = do
    input
    ident <- identifier
    semicolon
    return $ Input ident

outputInstr :: Parser Instr
outputInstr = do
    output
    expr <- expression
    semicolon
    return $ Output expr

assignmentInstr :: Parser Instr
assignmentInstr = do
    ident <- identifier
    equals
    expr <- expression
    semicolon
    return $ Assignment ident expr

parens :: Parser a -> Parser a
parens p = void (lexeme $ char '(') *> p <* void (lexeme $ char ')')

operator :: Parser Expr
operator = do
    a <- termTest
    chr <- oneOf "+-*/"
    b <- expression
    return $ Operator chr a b

term :: Parser Expr -> Parser Expr
term expr =   Constant <$> literal
          <|> Variable <$> identifier
          <|> parens expr

termTest = term termTest

expression :: Parser Expr
expression =   try operator
           <|> termTest
