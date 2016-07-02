module Runner (run, runBlock) where

import           Control.Monad.State
import qualified Data.Map as Map
import           Types

run :: Action () -> IO ()
run m = evalStateT m Map.empty

runBlock :: Block -> Action ()
runBlock [] = return ()
runBlock (x:xs) = do
    runInstr x
    runBlock xs

setVar :: Ident -> Value -> Action ()
setVar ident val = modify $ Map.insert ident val

getVar :: Ident -> Action Value
getVar var = do
    vars <- get
    case Map.lookup var vars of
        Nothing -> fail "variable " ++ unIdent var ++ " not found"
        Just val -> return val

checkBool :: Value -> Bool
checkBool (Value num) = num /= 0

runInstr :: Instr -> Action ()
runInstr (Output expr) = do
    value <- evalExpr expr
    liftIO $ print $ unValue $ value
runInstr (Input ident) = do
    input <- liftIO readLn
    setVar ident (Value input)
runInstr (Assignment ident expr) = do
    value <- evalExpr expr
    setVar ident value
runInstr (IfBlock cond block) = do
    bool <- evalExpr cond
    if checkBool bool
        then runBlock block
        else return ()
runInstr while@(WhileBlock cond block) = do
    bool <- evalExpr cond
    if checkBool bool
        then do
            runBlock block
            runInstr while
        else return ()

operators :: Map.Map Char (Integer -> Integer -> Integer)
operators = Map.fromList [ ('+', (+))
                         , ('-', (-))
                         , ('*', (*))
                         , ('/', div)
                         , ('%', mod)
                         , ('^', (^))
                         ]

evalExpr :: Expr -> Action Value
evalExpr (Constant val) = return val
evalExpr (Variable var) = getVar var
evalExpr (Operator char left right) = do
    Value lValue <- evalExpr left
    Value rValue <- evalExpr right
    let func = operators Map.! char
    return $ Value (func lValue rValue)
