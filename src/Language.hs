module Language
    ( Environment
    , ExprC (..)
    , ExprExt (..)
    , NativeFunction (..)
    , Value (..)
    ) where

import           Data.Map (Map)
import           Errors   (FWError (..))

-- The actual values.
data Value
  = NumV Integer
  | NilV
  | BoolV Bool
  | ConsV Value Value
  | FunctionV ExprC Environment

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
  | NegC ExprC
  | ConsC ExprC ExprC
  | IfC ExprC ExprC ExprC
  | LambdaC (Maybe String) ExprC
  | AppC ExprC (Maybe ExprC)
  | NativeC NativeFunction
  deriving (Show)

data NativeFunction
  = EnvNativeFunction (Environment -> Either FWError Value)

instance Show NativeFunction where
  show (EnvNativeFunction _) = "<ENF>"

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

type Environment = Map String Value
