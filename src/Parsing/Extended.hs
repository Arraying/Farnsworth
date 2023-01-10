module Parsing.Extended
    ( ExprExt (..)
    ) where

data ExprExt
  = NumExt Integer
  | NilExt
  | TrueExt
  | FalseExt
  | IdExt String
  | UnOpExt String ExprExt
  | BinOpExt String ExprExt ExprExt
  | IfExt ExprExt ExprExt ExprExt
  | ListExt [ExprExt]
  | AnonExt [String] ExprExt
  | AppExt ExprExt [ExprExt]
  deriving (Show)
