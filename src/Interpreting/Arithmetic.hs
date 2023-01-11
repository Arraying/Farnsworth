module Interpreting.Arithmetic
    ( minus
    , plus
    ) where

import           Errors   (FWError (..))
import           Language (Value (..))

plus :: Value -> Value -> Either FWError Value
plus (NumV l) (NumV r) = Right $ NumV $ l + r
plus _ _ = Left $ FWInterpError "Addition requires LHS and RHS to be numbers"

minus :: Value -> Value -> Either FWError Value
minus (NumV l) (NumV r) = Right $ NumV $ l - r
minus _ _ = Left $ FWInterpError "Subtraction requires LHS and RHS to be numbers"
