module Interpreting.Logic
    ( and'
    , eq
    , gt
    , gte
    , lt
    , lte
    , neq
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

eq :: Value -> Value -> Either FWError Value
eq (NumV l) (NumV r) = Right $ BoolV $ l == r
eq (BoolV l) (BoolV r) = Right $ BoolV $ l == r
eq _ _ = Left $ FWInterpError "Logical equals requires LHS and RHS to be both a number or boolean"

neq :: Value -> Value -> Either FWError Value
neq (NumV l) (NumV r) = Right $ BoolV $ l /= r
neq (BoolV l) (BoolV r) = Right $ BoolV $ l /= r
neq _ _ = Left $ FWInterpError "Logical not equals requires LHS and RHS to be both a number or boolean"

lt :: Value -> Value -> Either FWError Value
lt (NumV l) (NumV r) = Right $ BoolV $ l < r
lt _ _ = Left $ FWInterpError "Logical less-than requires LHS and RHS to be a number"

gt :: Value -> Value -> Either FWError Value
gt (NumV l) (NumV r) = Right $ BoolV $ l > r
gt _ _ = Left $ FWInterpError "Logical greater-than requires LHS and RHS to be a number"

lte :: Value -> Value -> Either FWError Value
lte (NumV l) (NumV r) = Right $ BoolV $ l <= r
lte _ _ = Left $ FWInterpError "Logical less-than or equalsrequires LHS and RHS to be a number"

gte :: Value -> Value -> Either FWError Value
gte (NumV l) (NumV r) = Right $ BoolV $ l >= r
gte _ _ = Left $ FWInterpError "Logical greater-than or equals requires LHS and RHS to be a number"
