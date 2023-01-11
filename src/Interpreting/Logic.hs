module Interpreting.Logic
    ( and'
    , not'
    , or'
    ) where

import           Errors   (FWError (..))
import           Language (Value (..))

not' :: Value -> Either FWError Value
not' (BoolV b) = Right $ BoolV $ not b
not' _         = Left $ FWInterpError "Logical negation requires a boolean"

and' :: Value -> Value -> Either FWError Value
and' (BoolV l) (BoolV r) = Right $ BoolV $ l && r
and' _ _ = Left $ FWInterpError "Logical conjunction requires LHS and RHS to be a boolean"

or' :: Value -> Value -> Either FWError Value
or' (BoolV l) (BoolV r) = Right $ BoolV $ l || r
or' _ _ = Left $ FWInterpError "Logical disjunction requires LHS and RHS to be a boolean"
