module Interpreting.Value
    ( Value (..)
    ) where

import           Data.Map        (Map)
import           Desugaring.Core (ExprC (..))

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
