module Parsing.Parser
    ( ExprExt
    , parseSExpr
    ) where

import           Common
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
parseSExpr (SNum n) = Right $ NumExt n
parseSExpr (SSym "Nil") = Right NilExt
parseSExpr (SSym "True") = Right TrueExt
parseSExpr (SSym "False") = Right FalseExt
parseSExpr (SList [SSym "+", l, r]) = binop "+" l r
parseSExpr (SList [SSym "-", l, r]) = binop "-" l r
parseSExpr (SList [SSym "*", l, r]) = binop "*" l r
parseSExpr (SList [SSym "/", l, r]) = binop "/" l r
parseSExpr (SList [SSym "&&", l, r]) = binop "&&" l r
parseSExpr (SList [SSym "||", l, r]) = binop "||" l r
parseSExpr (SList [SSym "==", l, r]) = binop "==" l r
parseSExpr (SList [SSym ">", l, r]) = binop ">" l r
parseSExpr (SList [SSym "<", l, r]) = binop "<" l r
parseSExpr (SList [SSym "if", c, t, f]) = Left $ FWSyntaxError "Not implemented" 
parseSExpr (SList (SSym "list" : xs)) = Left $ FWSyntaxError "Not implemented" 
parseSExpr _ = Left $ FWSyntaxError "Syntax error"

binop :: String -> SExpr -> SExpr -> Either FWError ExprExt
binop s l r = mapRight (\(l', r') -> Right $ BinOpExt s l' r') $ mapBin parseSExpr (l, r)
