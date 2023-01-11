module Language
    ( ExprC (..)
    , ExprExt (..)
    , Value (..)
    ) where

import           Data.Map (Map)

-- The actual values.
data Value
  = NumV Integer
  | NilV
  | BoolV Bool
  | ConsV Value Value
  | FunctionV ExprC (Map String Value)

instance Show Value where
  show (NumV n)        = show n
  show (NilV)          = "Nil"
  show (BoolV b)       = show b
  show (ConsV l r)     = (show l) ++ " : " ++ (show r)
  show (FunctionV _ _) = "Function"

-- The core language syntax.
data ExprC
  = NumC Integer
  | NilC
  | TrueC
  | FalseC
  | IdC String
  | HeadC ExprC
  | TailC ExprC
  | IsNilC ExprC
  | IsListC ExprC
  | NandC ExprC ExprC
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  | DivC ExprC ExprC
  | EqC ExprC ExprC
  | LtC ExprC ExprC
  | GtC ExprC ExprC -- Can be optimized to LtC but this is a substantial investment.
  | ConsC ExprC ExprC
  | IfC ExprC ExprC ExprC
  | LambdaC (Maybe String) ExprC
  | AppC ExprC (Maybe ExprC)
  deriving (Show)

-- The extended language syntax.
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
  | AnonFnExt [String] ExprExt
  | NamedFnExt String [String] ExprExt
  | AppExt ExprExt [ExprExt]
  deriving (Show)
