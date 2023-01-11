module Parsing.Parser
    ( ExprExt
    , parseSExpr
    ) where

import           Common
import           Errors        (FWError (..))
import           Language      (ExprExt (..))
import           Parsing.SExpr

parseSExpr :: SExpr -> Either FWError ExprExt
parseSExpr (SNum n) = Right $ NumExt n
parseSExpr (SSym "Nil") = Right NilExt
parseSExpr (SSym "True") = Right TrueExt
parseSExpr (SSym "False") = Right FalseExt
parseSExpr (SSym s)
  | s `notElem` reserved = Right $ IdExt s
parseSExpr (SList [SSym s, e])
  | s `elem` unOps = unOp s e
parseSExpr (SList [SSym s, l, r])
  | s `elem` binOps = binOp s l r
parseSExpr (SList [SSym "if", c, t, f]) = case mapMany parseSExpr [c, t, f] of
  Left e             -> Left e
  Right [c', t', f'] -> Right $ IfExt c' t' f'
  Right _            -> Left $ FWSyntaxError "Internal if-statement error"
parseSExpr (SList (SSym "list" : xs)) = mapRight (\xs' -> Right $ ListExt xs') $ mapMany parseSExpr xs
parseSExpr (SList [SSym "anon", SList a, b]) = mapRight (\a' -> mapRight (\b' -> Right $ AnonFnExt a' b') $ parseSExpr b) $ mapMany argParser a
parseSExpr (SList [SSym "fn", SSym s, SList a, b]) = mapRight (\a' -> mapRight (\b' -> Right $ NamedFnExt s a' b' ) $ parseSExpr b ) $ mapMany argParser a
parseSExpr (SList (x:xs)) = mapRight (\x' -> mapRight (\xs' -> Right $ AppExt x' xs') $ mapMany parseSExpr xs) $ parseSExpr x
parseSExpr sexpr = Left $ FWSyntaxError $ show sexpr

argParser :: SExpr -> Either FWError String
argParser sexpr = mapRight (\s -> case s of
      IdExt(s') -> Right s'
      _ -> Left $ FWSyntaxError ("Anonymous lambda argument needs to be string; found " ++ (show sexpr))) $ parseSExpr sexpr

unOp :: String -> SExpr -> Either FWError ExprExt
unOp s e = mapRight (\e' -> Right $ UnOpExt s e') $ parseSExpr e

binOp :: String -> SExpr -> SExpr -> Either FWError ExprExt
binOp s l r = mapRight (\(l', r') -> Right $ BinOpExt s l' r') $ mapBin parseSExpr (l, r)
