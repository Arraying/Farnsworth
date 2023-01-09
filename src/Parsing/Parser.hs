module Parsing.Parser
    ( ExprExt
    , parseSExpr
    ) where

import           Errors
import           Parsing.SExpr

data ExprExt
  = NumExt Integer
  | NilExt
  | TrueExt
  | FalseExt
  | UnOpExt String ExprExt
  | BinOpExt String ExprExt ExprExt
  | IfExt ExprExt ExprExt ExprExt
  | ListExt [ExprExt]
  deriving (Show)

-- TODO: Write parser.
parseSExpr :: SExpr -> Either FWError ExprExt
parseSExpr sexpr = Right NilExt
