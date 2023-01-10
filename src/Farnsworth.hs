module Farnsworth
    ( FWError (..)
    , run
    ) where

import           Common
import           Desugaring.Desugarer (ExprC, desugar)
import           Errors
import           Parsing.Parser       (parseSExpr)
import           Parsing.SExpr        (parseStr)

run :: String -> Either FWError ExprC
run str = mapRight desugar $ mapRight parseSExpr $ parseStr str
