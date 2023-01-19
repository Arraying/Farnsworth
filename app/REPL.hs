module REPL
    ( repl
    , replEval
    , replPrint
    ) where


import           Control.Monad (unless)
import           Farnsworth
import           Language      (Value)
import           System.IO

repl :: IO ()
repl = do
  input <- replRead
  unless (input == ":q")
    $ replPrint (replEval input) >> repl

replRead :: IO String
replRead = putStr "Farnsworth> " >> hFlush stdout >> getLine

replEval :: String -> Either FWError Value
replEval = run

replPrint :: Either FWError Value ->  IO ()
replPrint = either print print
