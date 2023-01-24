module Interpreting.Charaters
    ( lower
    , upper
    ) where

import           Data.Char (toLower, toUpper)
import           Errors    (FWError (..))
import           Language  (Value (..))

upper :: Value -> Either FWError Value
upper (CharV c) = Right $ CharV $ toUpper c
upper _         = Left $ FWInterpError "Upper requires a character"

lower :: Value -> Either FWError Value
lower (CharV c) = Right $ CharV $ toLower c
lower _         = Left $ FWInterpError "Lower requires a character"
