module Main where

import           Parser
import           Runner
import           System.Environment (getArgs)
import           Types

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (args !! 0)
    runCode src

runCode :: String -> IO ()
runCode = run . runBlock . parseCode
