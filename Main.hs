module Main where

import           Lexer
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
runCode = run . runBlock . parseBlock . lexer
