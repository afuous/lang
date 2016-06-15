module Runner (run, runBlock) where

import           Control.Monad.State
import qualified Data.Map as Map
import           Types

run :: Action -> IO ()
run m = evalStateT m Map.empty

runBlock :: Block -> Action
runBlock [] = return ()
runBlock (x:xs) = do
    runInstr x
    runBlock xs

setVar :: Ident -> Value -> Action
setVar ident val = modify $ Map.insert ident val

runInstr :: Instr -> Action
runInstr (Output expr) = do
    vars <- get
    liftIO $ print $ unValue $ evalExpr vars expr
runInstr (Input ident) = do
    input <- liftIO readLn
    setVar ident (Value input)
runInstr (Assignment ident expr) = do
    vars <- get
    setVar ident $ evalExpr vars expr

operators :: Map.Map Char (Integer -> Integer -> Integer)
operators = Map.fromList [ ('+', (+))
                         , ('-', (-))
                         , ('*', (*))
                         , ('/', div)
                         , ('^', (^))
                         ]

evalExpr :: Vars -> Expr -> Value
evalExpr _ (Constant val) = val
evalExpr vars (Variable var) =
    case Map.lookup var vars of
        Nothing -> error $ "variable " ++ unIdent var ++ " not found"
        Just val -> val
evalExpr vars (Operator char left right) =
    let func = operators Map.! char
        Value lValue = evalExpr vars left
        Value rValue = evalExpr vars right
    in Value (func lValue rValue)
