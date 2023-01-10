module Desugaring.Core
    ( ExprC (..)
    ) where

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