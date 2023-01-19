module Main
    ( main
    ) where

import           REPL               (repl, replEval, replPrint)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if not $ null args then do
    contents <- readFile $ head args
    replPrint $ replEval contents
  else repl
