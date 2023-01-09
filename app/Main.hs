module Main
    ( main
    ) where

import           Farnsworth
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args /= 0 then do
    contents <- readFile $ head args
    case run contents of
      Left e  -> print e
      Right e -> print e
  else do
    putStrLn "Please provide the source file to run!"
    return ()
