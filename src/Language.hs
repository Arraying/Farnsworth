module Language
    ( Environment
    , ExprC (..)
    , ExprExt (..)
    , NativeFunction (..)
    , Pat (..)
    , Stricter
    , Value (..)
    , binOps
    , reserved
    , unOps
    ) where

import           Data.Map (Map)
import           Errors   (FWError (..))

-- The actual values.
data Value
  = NumV Integer
  | CharV Char
  | NilV
  | BoolV Bool
  | ConsV Value Value
  | FunctionV ExprC Environment
  | ThunkV ExprC Environment

instance Show Value where
  show (NumV n)        = show n
  show (CharV c)       = show c
  show NilV            = "Nil"
  show (BoolV b)       = show b
  show (ConsV l r)     = show l ++ " : " ++ show r
  show (FunctionV _ _) = "Function"
  show (ThunkV _ _)    = "Thunk"

-- The core language syntax.
data ExprC
  = NumC Integer
  | CharC Char
  | NilC
  | TrueC
  | FalseC
  | IdC String
  | NegC ExprC
  | ConsC ExprC ExprC
  | IfC ExprC ExprC ExprC
  | MatchC ExprC [(Pat, ExprC)]
  | LambdaC (Maybe String) ExprC
  | AppC ExprC (Maybe ExprC)
  | NativeC NativeFunction
  deriving (Eq, Show)

data NativeFunction
  -- Environment will always contain stricted values.
  = EnvNativeFunction (Environment -> Either FWError Value)

instance Eq NativeFunction where
  (==) _ _ = True -- Not needed for basic unit tests.

instance Show NativeFunction where
  show (EnvNativeFunction _) = "<ENF>"

-- The extended language syntax.
data ExprExt
  = NumExt Integer
  | CharExt Char
  | StrExt String
  | NilExt
  | TrueExt
  | FalseExt
  | IdExt String
  | UnOpExt String ExprExt
  | BinOpExt String ExprExt ExprExt
  | IfExt ExprExt ExprExt ExprExt
  | ListExt [ExprExt]
  | MatchExt ExprExt [(Pat, ExprExt)]
  | AnonFnExt [String] ExprExt
  | NamedFnExt String [String] ExprExt
  | AppExt ExprExt [ExprExt]
  deriving (Eq, Show)

data Pat
  = NumP Integer
  | CharP Char
  | TrueP
  | FalseP
  | NilP
  | ConsP Pat Pat
  | IdP String
  deriving (Eq, Show)

type Environment = Map String Value
type Stricter = (Value -> Either FWError Value)

reserved :: [String]
reserved = unOps ++ binOps ++ ["if", "list", "\\", "fn", "match", "case"]

unOps :: [String]
unOps = ["-"]

binOps :: [String]
binOps = ["cons"]
