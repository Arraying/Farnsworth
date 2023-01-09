module Errors
    ( FWError (..)
    ) where

import           Text.Parsec (ParseError)

data FWError
  = FWParseError ParseError
  | FWSyntaxError String
  | FWInterpError String

instance Show FWError where
    show (FWParseError e)  = "Invalid syntax:\n" ++ (show e)
    show (FWSyntaxError s) = "Invalid syntax:\n" ++ s
    show (FWInterpError s) = "Runtime exception:\n" ++ s
