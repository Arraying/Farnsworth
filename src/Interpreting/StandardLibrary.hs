module Interpreting.StandardLibrary
    ( standardLibraryEnvironment
    ) where

import qualified Data.Map as Map
import           Errors   (FWError (..))
import           Language (Environment, ExprC (..), NativeFunction (..),
                           Value (..))

standardLibraryEnvironment :: Environment
standardLibraryEnvironment = Map.fromList [("one", FunctionV (LambdaC Nothing $ NativeC $ EnvNativeFunction one) Map.empty)]

one :: Environment -> Either FWError Value
one _ = Right $ NumV 1
