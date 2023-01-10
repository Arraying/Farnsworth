module REPL where


import           Control.Monad        (unless)
import           Desugaring.Desugarer (ExprC)
import           Farnsworth
import           System.IO

repl :: IO ()
repl = do
  input <- replRead
  unless (input == ":q")
    $ replPrint (replEval input) >> repl

replRead :: IO String
replRead = putStr "Farnsworth> " >> hFlush stdout >> getLine

replEval :: String -> Either FWError ExprC
replEval = run

replPrint :: Either FWError ExprC ->  IO ()
replPrint e = either print print e
