module Farnsworth
    ( FWError (..)
    , run
    ) where

import           Common
import           Errors
import           Parsing.Parser (ExprExt, parseSExpr)
import           Parsing.SExpr  (SExpr, parseStr)

run :: String -> Either FWError ExprExt
run str = mapRight parseSExpr $ parseStr str
