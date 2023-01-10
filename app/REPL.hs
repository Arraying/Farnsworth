module REPL where


import           Control.Monad  (unless)
import           Data.Either    (either)
import           Farnsworth
import           Parsing.Parser (ExprExt)
import           System.IO

repl :: IO ()
repl = do
  input <- replRead
  unless (input == ":q")
    $ replPrint (replEval input) >> repl

replRead :: IO String
replRead = putStr "Farnsworth> " >> hFlush stdout >> getLine

replEval :: String -> Either FWError ExprExt
replEval = run

replPrint :: Either FWError ExprExt ->  IO ()
replPrint e = either print print e
