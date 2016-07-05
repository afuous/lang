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

operators = [ ['+', '-']
            , ['*', '/', '%']
            , ['^'] -- TODO: this is right associative
            ]

--operatorsWith :: [[Char]] -> Parser Expr
--operatorsWith [] = term
--operatorsWith (x:xs) = try (do
--    a <- operatorsWith xs
--    chr <- oneOf x
--    b <- expression
--    return $ Operator chr a b) <|> operatorsWith xs
--
--operator = operatorsWith operators

operator :: Parser Expr
operator = do
    a <- term
    chr <- choice $ map (try . oneOf) operators
    b <- expression
    return $ Operator chr a b

term :: Parser Expr
term = term' term
  where
    term' :: Parser Expr -> Parser Expr
    term' expr =   Constant <$> literal
               <|> Variable <$> identifier
               <|> parens expression

expression :: Parser Expr
expression =   try operator
           <|> term
