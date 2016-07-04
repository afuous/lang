module Test where

import           Control.Monad (msum)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.List (elemIndex)
import           Data.Maybe (isJust)
import           Types


type Parser a = MaybeT (State [Token]) a

liftMaybe = MaybeT . return

getNotEmpty :: Parser [Token]
getNotEmpty = do
    xs <- get
    if null xs
        then liftMaybe Nothing
        else return xs

getToken :: Parser Token
getToken = do
    tokens <- getNotEmpty
    modify $ tail
    return $ head tokens

expectToken :: Token -> Parser ()
expectToken t = do
    token <- getToken
    if token == t
        then return ()
        else liftMaybe Nothing

expectIdent :: Parser Ident
expectIdent = do
    token <- getToken
    case token of
        (TIdent ident) -> return ident
        _ -> liftMaybe Nothing

expectExprUntil :: Token -> Parser Expr
expectExprUntil token = do
    tokens <- get
    index <- liftMaybe $ token `elemIndex` tokens
    modify $ drop (index + 1)
    liftMaybe $ runParser (take index tokens) parseExpr

parseExpr :: Parser Expr
parseExpr = do

parseInput :: Parser Instr
parseInput = do
    expectToken TInput
    ident <- expectIdent
    expectToken TSemicolon
    return $ Input ident

instrParsers :: [Parser Instr]
instrParsers = [parseInput]

runParser :: [Token] -> Parser a -> (Maybe a, [Token])
runParser tokens parser = runState (runMaybeT parser) tokens

parse :: [Token] -> Maybe Block
parse [] = Just []
parse tokens =
    let parsed = map (runParser tokens) instrParsers in
    case filter (\(v, tokens) -> isJust v) parsed of
        [] -> Nothing
        (((Just instr), ts):_) -> fmap (instr:) (parse ts)

main :: IO ()
main = do
    print $ parse $ [TInput, TIdent (Ident "hello"), TSemicolon, TInput, TIdent (Ident "a"), TSemicolon]

