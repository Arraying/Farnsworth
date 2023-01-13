module Interpreting.NativeFunctions
    ( function0
    , function1
    , function2
    ) where

import           Common   (mapRight)
import           Data.Map (empty, (!))
import           Errors   (FWError (..))
import           Language (Environment, ExprC (..), NativeFunction (..),
                           Stricter, Value (..))

function0 :: Either FWError Value -> Value
function0 e = FunctionV (LambdaC Nothing $ NativeC $ EnvNativeFunction (\_ -> e)) empty

function1 :: Stricter -> (Value -> Either FWError Value) -> Value
function1 strict f = FunctionV (LambdaC (Just "1")
  $ NativeC $ EnvNativeFunction (\env -> mapRight f $ mapRight strict $ envV "1" env)) empty

function2 :: Stricter -> (Value -> Value -> Either FWError Value) -> Value
function2 strict f = FunctionV (LambdaC (Just "1") $ LambdaC (Just "2")
  $ NativeC $ EnvNativeFunction (\env -> mapRight (\l -> mapRight (\r -> f l r) $ strictV env "2") $ strictV env "1")) empty
  where
    strictV env s = mapRight strict $ envV s env

envV :: String -> Environment -> Either FWError Value
envV s env = Right $ env ! s
