module Main where

import           Parser (parseCode)
import           Runner (run, runBlock)
import           System.Environment (getArgs)
import           Types

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (head args)
    runCode src

runCode :: String -> IO ()
runCode code = case parseCode code of
    Right block -> run $ runBlock block
    Left err    -> fail $ show err
