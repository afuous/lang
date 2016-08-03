module Parser (parseCode) where

import           Control.Monad (void)
import           Operators
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.String (Parser)
import           Types

parseCode :: String -> Either ParseError Block
parseCode code = parse (linebreak *> block <* eof) "syntax error" code

block :: Parser Block
block = many $   try inputInstr
             <|> outputInstr
             <|> try assignmentInstr
             <|> try ifElseInstr
             <|> whileInstr
             <|> callInstr

whitespace :: Parser ()
whitespace = void $ many $ oneOf "n\t "

-- TODO: clean up whitespace that allows newlines and whitespace that does not
lexeme :: Parser a -> Parser a
lexeme p = p <* void (many (oneOf "\t "))

literal :: Parser Value
literal = lexeme $ LangInt <$> read <$> many1 digit
               <|> LangStr <$> (char '"' *> many letter <* char '"')
               <|> reservedWord "True" *> pure (LangBool True)
               <|> reservedWord "False" *> pure (LangBool False)

identifier :: Parser Ident
identifier = lexeme $ Ident <$> many1 letter

reservedWord :: String -> Parser ()
reservedWord s = void $ lexeme $ string s

comment :: Parser ()
comment = do
  void $ char '#'
  void $ many $ noneOf "\n"

linebreak = many $ void (oneOf "\n\t ") <|> comment

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
    reservedWord "="
    expr <- expression
    linebreak
    return $ Assignment ident expr

ifElseInstr :: Parser Instr
ifElseInstr = do
    reservedWord "if"
    cond <- expression
    reservedWord "{"
    linebreak
    whenTrue <- block
    reservedWord "}"
    whenFalse <- optionMaybe $ try $ do
        reservedWord "else"
        reservedWord "{"
        linebreak
        b <- block
        reservedWord "}"
        return b
    linebreak
    return $ IfElseBlock cond whenTrue whenFalse

whileInstr :: Parser Instr
whileInstr = do
    reservedWord "while"
    cond <- expression
    reservedWord "{"
    linebreak
    instrs <- block
    reservedWord "}"
    linebreak
    return $ WhileBlock cond instrs

callInstr :: Parser Instr
callInstr = do
  reservedWord "call"
  (Call <$> identifier <*> many expression) <* linebreak

operatorTable = map (map toParsec) (reverse operators)
  where
    toParsec op = Infix (reservedWord (symbol op) >> return (Operator op))
                        (parsecAssoc (assoc op))
    parsecAssoc LAssoc = AssocLeft
    parsecAssoc RAssoc = AssocRight
    parsecAssoc NoAssoc = AssocNone

expression :: Parser Expr
expression = buildExpressionParser operatorTable term

lambda :: Parser Expr
lambda = do
  reservedWord "("
  args <- many identifier
  reservedWord ")"
  reservedWord "=>"
  reservedWord "{"
  linebreak
  instrs <- block
  reservedWord "}"
  return $ FuncDef args instrs

parens :: Parser a -> Parser a
parens p = reservedWord "(" *> p <* reservedWord ")"

term :: Parser Expr
term =   try lambda
     <|> parens expression
     <|> try (Constant <$> literal)
     <|> Variable <$> identifier
