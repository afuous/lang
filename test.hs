module Test where

import           Control.Monad (msum)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.List (elemIndex)
import           Data.Maybe (fromJust, isJust)
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
    expr <- liftMaybe $ parseExpr (take index tokens)
    return expr

operators :: [[Char]]
operators = [ ['+', '-']
            , ['*', '/', '%']
            , ['/']
            ]

-- I give up
parseExpr :: [Token] -> Maybe Expr
parseExpr [] = Nothing
parseExpr [TLiteral value] = Just $ Constant value
parseExpr [TIdent ident] = Just $ Variable ident
parseExpr xs =
    let rev = reverse xs
    in case findOperator rev of
        Nothing -> if head xs == TLeftParen && last xs == TRightParen
            then parseExpr $ init $ tail xs
            else error "invalid expression"
        Just pos -> let right = reverse $ take pos rev
                        left = reverse $ drop (pos + 1) rev
                        (TOperator char) = rev !! pos
                    in Just $ Operator char (fromJust $ parseExpr left) (fromJust $ parseExpr right)
  where

    findOperator :: [Token] -> Maybe Int
    findOperator rev = msum $ map (walk rev 0) operators

    walk :: [Token] -> Int -> [Char] -> Maybe Int
    walk [] _ _ = Nothing
    walk _ n _ | n < 0 = Nothing
    walk (TRightParen:xs) n ops = fmap (+1) $ walk xs (n + 1) ops
    walk (TLeftParen:xs) n ops = fmap (+1) $ walk xs (n - 1) ops
    walk ((TOperator op):xs) 0 ops | op `elem` ops = Just 0
    walk (x:xs) n ops = fmap (+1) $ walk xs n ops

parseInput :: Parser Instr
parseInput = do
    expectToken TInput
    ident <- expectIdent
    expectToken TSemicolon
    return $ Input ident

parseOutput :: Parser Instr
parseOutput = do
    expectToken TOutput
    expr <- expectExprUntil TSemicolon
    return $ Output expr

parseAssignment :: Parser Instr
parseAssignment = do
    ident <- expectIdent
    expectToken TEquals
    expr <- expectExprUntil TSemicolon
    return $ Assignment ident expr

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

