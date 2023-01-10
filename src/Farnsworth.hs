module Farnsworth
    ( FWError (..)
    , run
    ) where

import           Common
import           Desugaring.Desugarer     (desugar)
import           Errors
import           Interpreting.Interpreter (interpret)
import           Interpreting.Value       (Value)
import           Parsing.Parser           (parseSExpr)
import           Parsing.SExpr            (parseStr)

run :: String -> Either FWError Value
run str = mapRight interpret $ mapRight desugar $ mapRight parseSExpr $ parseStr str
