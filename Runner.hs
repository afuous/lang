module Runner (run, runBlock) where

import           Control.Monad.State
import qualified Data.Map as Map
import           Types

run :: StateT Variables IO () -> IO ()
run m = evalStateT m Map.empty

runBlock :: Block -> StateT Variables IO ()
runBlock [] = return ()
runBlock (x:xs) = do
    runInstruction x
    runBlock xs

setVariable :: Identifier -> Value -> StateT Variables IO ()
setVariable ident val = modify $ Map.insert ident val

runInstruction :: Instruction -> StateT Variables IO ()
runInstruction (Output expr) = do
    vars <- get
    liftIO $ print $ unValue $ evaluateExpression vars expr
runInstruction (Input ident) = do
    input <- liftIO readLn
    setVariable ident (Value input)
runInstruction (Assignment ident expr) = do
    vars <- get
    setVariable ident $ evaluateExpression vars expr


operators :: Map.Map Char (Integer -> Integer -> Integer)
operators = Map.fromList [ ('+', (+))
                         , ('-', (-))
                         , ('*', (*))
                         , ('/', div)
                         , ('^', (^))
                         ]

evaluateExpression :: Variables -> Expression -> Value
evaluateExpression _ (Constant val) = val
evaluateExpression vars (Variable var) =
    case Map.lookup var vars of
        Nothing -> error "fail" -- "variable " ++ unIdentifier var ++ " not found"
        Just val -> val
evaluateExpression vars (Operator char left right) =
    let func = operators Map.! char
        Value lValue = evaluateExpression vars left
        Value rValue = evaluateExpression vars right
    in Value (func lValue rValue)
