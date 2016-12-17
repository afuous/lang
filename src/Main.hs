module Main where

import Compiler (compile)
import Parser (parseCode)
import System.Environment (getArgs)
import Types

main :: IO ()
main = do
  args <- getArgs
  src <- readFile (head args)
  case parseCode src of
    Left err -> fail (show err)
    Right block -> putStrLn (compile block)
