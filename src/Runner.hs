module Runner (run, runBlock) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Map as Map
import           Operators
import           Types

run :: Action () -> IO ()
run m = void $ runEitherT (evalStateT m [builtins])

builtins :: Map.Map Ident Value
builtins = Map.fromList
  [ (Ident "print", BuiltInFunc (\[arg] -> output arg >> return LangNull))
  , (Ident "input", BuiltInFunc (\[] -> LangStr <$> liftIO getLine))
  , (Ident "parse_int", BuiltInFunc (\[LangStr s] -> return (LangInt (read s))))
  ]

output :: Value -> Action ()
output val = liftIO $ case val of
  LangInt num -> print num
  LangStr str -> putStrLn str
  LangBool bool -> print bool
  LangNull -> putStrLn "Null"
  BuiltInFunc _ -> putStrLn "<built in function>"

runBlock :: Block -> Action ()
runBlock block = do
  modify (Map.empty:)
  forM_ block runInstr
  modify tail

runFunc :: Block -> Map.Map Ident Value -> IO Value
runFunc block vars = do
  result <- runEitherT (evalStateT (runBlock block) [builtins, vars])
  return $ case result of
    Left val -> val
    Right _ -> LangNull

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
runInstr (Call func args) = void $ executeFunc func args
runInstr (Return expr) = evalExpr expr >>= stop

stop :: Value -> Action ()
stop = StateT . const . EitherT . return . Left

evalExpr :: Expr -> Action Value
evalExpr (Constant val) = return val
evalExpr (Variable var) = getVar var
evalExpr (Operator (Op _ f _) left right) = do
  lValue <- evalExpr left
  rValue <- evalExpr right
  return $ f lValue rValue
evalExpr (FuncDef args block) = return $ LangFunc args block
evalExpr (FuncCall func args) = executeFunc func args

executeFunc :: Expr -> [Expr] -> Action Value
executeFunc func args = do
  expr <- evalExpr func
  case expr of
    LangFunc argNames block ->
      if length args /= length argNames
        then fail "wrong number of arguments"
        else do
          argValues <- mapM evalExpr args
          liftIO $ runFunc block (Map.fromList (zip argNames argValues))
    BuiltInFunc f -> do
      argValues <- mapM evalExpr args
      f argValues
