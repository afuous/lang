module Parser (parseCode) where

import           Control.Monad (void)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
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

reservedWord :: String -> Parser ()
reservedWord s = void $ lexeme $ string s

reservedSymbol :: Char -> Parser ()
reservedSymbol c = void $ lexeme $ char c

inputInstr :: Parser Instr
inputInstr = do
    reservedWord "input"
    ident <- identifier
    reservedSymbol ';'
    return $ Input ident

outputInstr :: Parser Instr
outputInstr = do
    reservedWord "output"
    expr <- expression
    reservedSymbol ';'
    return $ Output expr

assignmentInstr :: Parser Instr
assignmentInstr = do
    ident <- identifier
    reservedSymbol '='
    expr <- expression
    reservedSymbol ';'
    return $ Assignment ident expr

op = reservedSymbol

operators = [ [ Infix (op '^' >> return (Operator '^')) AssocRight ]
            , [ Infix (op '*' >> return (Operator '*')) AssocLeft
              , Infix (op '/' >> return (Operator '/')) AssocLeft
              , Infix (op '%' >> return (Operator '%')) AssocLeft ]
            , [ Infix (op '+' >> return (Operator '+')) AssocLeft
              , Infix (op '-' >> return (Operator '-')) AssocLeft ]
            ]

expression :: Parser Expr
expression = buildExpressionParser operators term

parens :: Parser a -> Parser a
parens p = reservedSymbol '(' *> p <* reservedSymbol ')'

term :: Parser Expr
term =   parens expression
     <|> Variable <$> identifier
     <|> Constant <$> literal
