module Interpreting.Arithmetic
    ( divide
    , minus
    , multiply
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

multiply :: Value -> Value -> Either FWError Value
multiply (NumV l) (NumV r) = Right $ NumV $ l * r
multiply _ _ = Left $ FWInterpError "Multiplication requires LHS and RHS to be numbers"

divide :: Value -> Value -> Either FWError Value
divide (NumV _) (NumV 0) = Left $ FWInterpError "Division by zero error"
divide (NumV l) (NumV r) = Right $ NumV $ l `div` r
divide _ _ = Left $ FWInterpError "Division requires LHS and RHS to be numbers"
