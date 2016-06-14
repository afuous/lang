module Main where

import Types
import           Lexer
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (args !! 0)
    let tokens = lexer src
    print tokens
