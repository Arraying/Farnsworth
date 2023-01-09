module Farnsworth
    ( FWError (..)
    , run
    ) where

import           Errors
import           Parsing.Parser (ExprExt, parseSExpr)
import           Parsing.SExpr  (SExpr, parseStr)

run :: String -> Either FWError SExpr
run str = parseStr str

mapRight :: (a -> Either FWError b) -> Either FWError a -> Either FWError b
mapRight f (Right x) = f x
mapRight _ (Left x)  = Left x
