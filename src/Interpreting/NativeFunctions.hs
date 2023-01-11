module Interpreting.NativeFunctions
    ( function0
    , function1
    , function2
    ) where

import           Data.Map (empty, (!))
import           Errors   (FWError (..))
import           Language (Environment, ExprC (..), NativeFunction (..),
                           Value (..))

function0 :: Either FWError Value -> Value
function0 e = FunctionV (LambdaC Nothing $ NativeC $ EnvNativeFunction (\_ -> e)) empty

function1 :: (Value -> Either FWError Value) -> Value
function1 f = FunctionV (LambdaC (Just "1")
  $ NativeC $ EnvNativeFunction (\env -> f $ env ! "1")) empty

function2 :: (Value -> Value -> Either FWError Value) -> Value
function2 f = FunctionV (LambdaC (Just "1") $ LambdaC (Just "2")
  $ NativeC $ EnvNativeFunction (\env -> f (env ! "1") (env ! "2"))) empty


