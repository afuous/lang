module Runner (run, runBlock) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map as Map
import           Operators
import           Types

run :: Action () -> IO ()
run m = evalStateT m []

runBlock :: Block -> Action ()
runBlock b = do
    modify (Map.empty:)
    runBlock' b
    modify tail
  where
    runBlock' [] = return ()
    runBlock' (x:xs) = do
        runInstr x
        runBlock' xs

setVar :: Ident -> Value -> Action ()
setVar ident val = do
    vars <- get
    case set' vars of
        Just m -> put m
        Nothing -> put $ (Map.insert ident val (head vars)) : tail vars
  where
    set' :: Vars -> Maybe Vars
    set' [] = Nothing
    set' (x:xs) = if ident `Map.member` x
        then Just $ (Map.insert ident val x) : xs
        else fmap (x:) (set' xs)

getVar :: Ident -> Action Value
getVar var = do
    vars <- get
    get' vars
  where
    get' [] = fail $ "variable " ++ unIdent var ++ " not found"
    get' (x:xs) = case Map.lookup var x of
        Nothing -> get' xs
        Just val -> return val

checkBool :: Value -> Bool
checkBool (LangInt num) = num /= 0

runInstr :: Instr -> Action ()
runInstr (Output expr) = do
    value <- evalExpr expr
    case value of
      LangInt num -> liftIO $ print num
      LangStr str -> liftIO $ putStrLn str
runInstr (Input ident) = do
    input <- liftIO readLn
    setVar ident (LangInt input)
runInstr (Assignment ident expr) = do
    value <- evalExpr expr
    setVar ident value
runInstr (IfElseBlock cond whenTrue whenFalse) = do
    bool <- evalExpr cond
    if checkBool bool
        then runBlock whenTrue
        else case whenFalse of
            Nothing -> return ()
            Just b  -> runBlock b
runInstr while@(WhileBlock cond block) = do
    bool <- evalExpr cond
    if checkBool bool
        then do
            runBlock block
            runInstr while
        else return ()

evalExpr :: Expr -> Action Value
evalExpr (Constant val) = return val
evalExpr (Variable var) = getVar var
evalExpr (Operator (Op _ f _) left right) = do
    lValue <- evalExpr left
    rValue <- evalExpr right
    return $ f lValue rValue
