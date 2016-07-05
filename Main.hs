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
runCode code = case parseCode code of
    Right block -> do
        print block
        run $ runBlock block
    Left err    -> fail $ show err
