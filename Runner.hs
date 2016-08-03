module Runner (run, runBlock) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map as Map
import           Operators
import           Types

run :: Action () -> IO ()
run m = evalStateT m []

runBlock :: Block -> Action ()
runBlock block = do
  modify (Map.empty:)
  runBlock' block
  modify tail
 where
  runBlock' [] = return ()
  runBlock' (x:xs) = do
    runInstr x
    runBlock' xs

runBlockWith :: Block -> Map.Map Ident Value -> Action ()
runBlockWith block vars = do
  scope <- get
  put [vars]
  runBlock block
  put scope

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

runInstr :: Instr -> Action ()
runInstr (Output expr) = do
  value <- evalExpr expr
  case value of
    LangInt num -> liftIO $ print num
    LangStr str -> liftIO $ putStrLn str
    LangBool bool -> liftIO $ print bool
runInstr (Input ident) = do
  input <- liftIO readLn
  setVar ident (LangInt input)
runInstr (Assignment ident expr) = do
  value <- evalExpr expr
  setVar ident value
runInstr (IfElseBlock cond whenTrue whenFalse) = do
  LangBool bool <- evalExpr cond
  if bool
    then runBlock whenTrue
    else case whenFalse of
      Nothing -> return ()
      Just b  -> runBlock b
runInstr while@(WhileBlock cond block) = do
  LangBool bool <- evalExpr cond
  if bool
    then do
      runBlock block
      runInstr while
    else return ()
runInstr (Call name args) = do
  LangFunc argNames block <- getVar name
  if length args /= length argNames
    then fail "wrong number of arguments"
    else do
      argValues <- mapM evalExpr args
      runBlockWith block (Map.fromList (zip argNames argValues))

evalExpr :: Expr -> Action Value
evalExpr (Constant val) = return val
evalExpr (Variable var) = getVar var
evalExpr (Operator (Op _ f _) left right) = do
  lValue <- evalExpr left
  rValue <- evalExpr right
  return $ f lValue rValue
evalExpr (FuncDef args block) = return $ LangFunc args block
