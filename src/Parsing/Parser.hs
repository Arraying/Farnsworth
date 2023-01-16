module Parsing.Parser
    ( ExprExt
    , parseSExpr
    ) where

import           Errors        (FWError (..))
import           Language      (ExprExt (..), binOps, reserved, unOps)
import           Parsing.SExpr

parseSExpr :: SExpr -> Either FWError ExprExt
parseSExpr (SNum n) = Right $ NumExt n
parseSExpr (SSym "Nil") = Right NilExt
parseSExpr (SSym "True") = Right TrueExt
parseSExpr (SSym "False") = Right FalseExt
parseSExpr (SSym s)
  | s `notElem` reserved || s == "-" = Right $ IdExt s
parseSExpr (SList [SSym s, e])
  | s `elem` unOps = do
    e' <- parseSExpr e
    Right $ UnOpExt s e'
parseSExpr (SList [SSym s, l, r])
  | s `elem` binOps = do
    l' <- parseSExpr l
    r' <- parseSExpr r
    Right $ BinOpExt s l' r'
parseSExpr (SList [SSym "if", c, t, f]) = do
  c' <- parseSExpr c
  t' <- parseSExpr t
  f' <- parseSExpr f
  Right $ IfExt c' t' f'
parseSExpr (SList (SSym "list" : xs)) = do
  xs' <- traverse parseSExpr xs
  Right $ ListExt xs'
parseSExpr (SList [SSym "\\", SList a, b]) = do
  a' <- traverse argParser a
  b' <- parseSExpr b
  Right $ AnonFnExt a' b'
parseSExpr (SList [SSym "fn", SSym s, SList a, b]) = do
  a' <- traverse argParser a
  b' <- parseSExpr b
  Right $ NamedFnExt s a' b'
parseSExpr (SList (x:xs)) = do
  x' <- parseSExpr x
  xs' <- traverse parseSExpr xs
  Right $ AppExt x' xs'
parseSExpr sexpr = Left $ FWSyntaxError $ show sexpr

argParser :: SExpr -> Either FWError String
argParser sexpr = do
  s <- parseSExpr sexpr
  case s of
    IdExt(s') -> Right s'
    _ -> Left $ FWSyntaxError ("Anonymous lambda argument needs to be string; found " ++ (show sexpr))
