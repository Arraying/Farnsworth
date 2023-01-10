module Main
    ( main
    ) where

import           REPL
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args /= 0 then do
    contents <- readFile $ head args
    replPrint $ replEval contents
  else repl
