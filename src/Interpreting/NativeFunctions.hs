module Interpreting.NativeFunctions
    ( function0
    , function1
    , function2
    ) where

import           Data.Map (empty, (!))
import           Errors   (FWError (..))
import           Language (Environment, ExprC (..), NativeFunction (..),
                           Stricter, Value (..))

function0 :: Either FWError Value -> Value
function0 e = FunctionV (LambdaC Nothing $ NativeC $ EnvNativeFunction (\_ -> e)) empty

function1 :: Stricter -> (Value -> Either FWError Value) -> Value
function1 strict f = FunctionV (LambdaC (Just "1") $ NativeC $ EnvNativeFunction worker) empty
  where
    worker :: Environment -> Either FWError Value
    worker env = do
      e <- envV "1" env >>= strict
      f e

function2 :: Stricter -> (Value -> Value -> Either FWError Value) -> Value
function2 strict f = FunctionV (LambdaC (Just "1") $ LambdaC (Just "2") $ NativeC $ EnvNativeFunction worker) empty
  where
    worker :: Environment -> Either FWError Value
    worker env = do
      l <- envV "1" env >>= strict
      r <- envV "2" env >>= strict
      f l r

envV :: String -> Environment -> Either FWError Value
envV s env = Right $ env ! s
