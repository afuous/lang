module Main where

import           Lexer
import           Parser
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (args !! 0)
    let tokens = lexer src
    print $ parseBlock tokens
