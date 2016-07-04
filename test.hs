module Test where

import           Control.Monad.Trans.Maybe
import           Control.Monad.State
import           Types


type Parser a = MaybeT (State [Token]) a

liftMaybe = MaybeT . return

getNotEmpty :: Parser [Token]
getNotEmpty = do
    xs <- get
    if null xs
        then liftMaybe Nothing
        else return xs

expectToken :: Token -> Parser ()
expectToken token = do
    tokens <- getNotEmpty
    modify $ tail
    if head tokens == token
        then return ()
        else liftMaybe Nothing

expectIdent :: Parser Ident
expectIdent = do
    tokens <- get
    modify $ tail
    case head tokens of
        (TIdent ident) -> return ident
        _ -> liftMaybe Nothing

parseInput :: Parser Instr
parseInput = do
    expectToken TInput
    ident <- expectIdent
    expectToken TSemicolon
    return $ Input ident

main :: IO ()
main = do
    print $ runState (runMaybeT parseInput) [TInput, TIdent (Ident "hello"), TSemicolon]

