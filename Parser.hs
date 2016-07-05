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
block = many $   try inputInstr
             <|> outputInstr
             <|> try assignmentInstr
             <|> try ifInstr
             <|> whileInstr

whitespace :: Parser ()
whitespace = void $ many $ oneOf "n\t "

-- TODO: clean up whitespace that allows newlines and whitespace that does not
lexeme :: Parser a -> Parser a
lexeme p = p <* void (many (oneOf "\t "))

literal :: Parser Value
literal = lexeme $ Value <$> read <$> many1 digit

identifier :: Parser Ident
identifier = lexeme $ Ident <$> many1 letter

reservedWord :: String -> Parser ()
reservedWord s = void $ lexeme $ string s

reservedSymbol :: Char -> Parser ()
reservedSymbol c = void $ lexeme $ char c

linebreak = reservedSymbol '\n'

inputInstr :: Parser Instr
inputInstr = do
    reservedWord "input"
    ident <- identifier
    linebreak
    return $ Input ident

outputInstr :: Parser Instr
outputInstr = do
    reservedWord "output"
    expr <- expression
    linebreak
    return $ Output expr

assignmentInstr :: Parser Instr
assignmentInstr = do
    ident <- identifier
    reservedSymbol '='
    expr <- expression
    linebreak
    return $ Assignment ident expr

ifInstr :: Parser Instr
ifInstr = do
    reservedWord "if"
    cond <- expression
    reservedSymbol '{'
    linebreak
    instrs <- block
    reservedSymbol '}'
    linebreak
    return $ IfBlock cond instrs

whileInstr :: Parser Instr
whileInstr = do
    reservedWord "while"
    cond <- expression
    reservedSymbol '{'
    linebreak
    instrs <- block
    reservedSymbol '}'
    linebreak
    return $ WhileBlock cond instrs

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
