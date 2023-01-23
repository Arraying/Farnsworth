{-# LANGUAGE OverloadedStrings #-}

module REPL2
    ( evalAndPrint
    , repl2
    ) where

import           Byline                 (BylineT, askLn, bold, cyan, fg, green,
                                         red, runBylineT, sayLn, text, yellow)
import           Control.Monad          (unless, void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Function          ((&))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Farnsworth             (run)

repl2 :: IO ()
repl2 = prelude >> loop

prelude :: IO ()
prelude = cli $ sayLn $ "\nWelcome to the " <> ("Farnsworth" & fg yellow & bold) <> " REPL!\nPlease type your code or " <> (":q" & fg cyan) <> " to quit\n"

loop :: IO ()
loop = do
  x <- runBylineT $ askLn ("Farnsworth> " & fg cyan) Nothing
  unless (x == Just ":q") $ process x >> loop
  where
    process :: Maybe Text -> IO ()
    process Nothing   = return ()
    process (Just x') = evalAndPrint $ Text.unpack x'

evalAndPrint :: String -> IO ()
evalAndPrint code = case run code of
  (Left e)  -> cli $ sayLn $ text (Text.pack $ show e) & fg red
  (Right v) -> cli $ sayLn $ text (Text.pack $ show v) & fg green

cli :: (MonadIO f, Control.Monad.Catch.MonadMask f) => BylineT f a -> f ()
cli x = void $ runBylineT x
