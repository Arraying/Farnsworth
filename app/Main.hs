module Main
    ( main
    ) where

import           REPL2              (evalAndPrint, repl2)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if not $ null args then do
    contents <- readFile $ head args
    evalAndPrint contents
  else repl2
