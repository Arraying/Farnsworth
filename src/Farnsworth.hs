module Farnsworth
    ( FWError (..)
    , run
    ) where

import           Desugaring.Desugarer     (desugar)
import           Errors
import           Interpreting.Interpreter (interpret)
import           Language                 (Value)
import           Parsing.Parser           (parseSExpr)
import           Parsing.SExpr            (parseStr)

run :: String -> Either FWError Value
run str = parseStr str >>= parseSExpr >>= desugar >>= interpret
