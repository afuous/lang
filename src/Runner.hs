module Runner (run, runBlock) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Map as Map
import Operators
import Types

run :: Action () -> IO ()
run m = void $ runEitherT (evalStateT m [])

output :: Value -> Action ()
output val = liftIO $ case val of
  LangInt num -> print num
  LangStr str -> putStrLn str
  LangBool bool -> print bool
  LangNull -> putStrLn "Null"

runBlock :: Block -> Action ()
runBlock block = do
  modify (Map.empty:)
  forM_ block runInstr
  modify tail

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
  when bool $ do
    runBlock block
    runInstr while
runInstr (OutputInstr expr) = evalExpr expr >>= output
runInstr (InputInstr ident) = do
  input <- liftIO $ readLn
  setVar ident (LangInt input)

evalExpr :: Expr -> Action Value
evalExpr (Constant val) = return val
evalExpr (Variable var) = getVar var
evalExpr (Operator (Op _ f _) left right) = do
  lValue <- evalExpr left
  rValue <- evalExpr right
  return $ f lValue rValue
