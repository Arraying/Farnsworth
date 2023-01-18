module Main
    ( main
    ) where

import           REPL
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if not $ null args then do
    contents <- readFile $ head args
    replPrint $ replEval contents
  else repl
