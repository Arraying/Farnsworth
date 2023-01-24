module Parsing.Parser
    ( ExprExt
    , parseSExpr
    ) where

import           Errors        (FWError (..))
import           Language      (ExprExt (..), Pat (..), binOps, reserved, unOps)
import           Parsing.SExpr (SExpr (..))

parseSExpr :: SExpr -> Either FWError ExprExt
parseSExpr (SNum n) = Right $ NumExt n
parseSExpr (SChar c) = Right $ CharExt c
parseSExpr (SStr s) = Right $ StrExt s
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
parseSExpr e@(SList (SSym "if" : _)) = Left $ FWSyntaxError ("Invalid use of if: " ++ show e)
parseSExpr (SList (SSym "list" : xs)) = do
  xs' <- traverse parseSExpr xs
  Right $ ListExt xs'
parseSExpr (SList (SSym "match" : x : xs)) = do
  x' <- parseSExpr x
  xs' <- traverse parseCase xs
  Right $ MatchExt x' xs'
  where
    parseCase :: SExpr -> Either FWError (Pat, ExprExt)
    parseCase (SList [SSym "case", p, e]) = do
      p' <- parsePat p
      e' <- parseSExpr e
      Right (p', e')
    parseCase c                           = Left $ FWSyntaxError ("Malformed case: " ++ show c)
    parsePat :: SExpr -> Either FWError Pat
    parsePat (SNum n)                    = Right $ NumP n
    parsePat (SSym "True")               = Right TrueP
    parsePat (SSym "False")              = Right FalseP
    parsePat (SSym "Nil")                = Right NilP
    parsePat (SList [SSym "cons", h, t]) = do
      h' <- parsePat h
      t' <- parsePat t
      Right $ ConsP h' t'
    parsePat (SSym s) = Right $ IdP s
    parsePat pat = Left $ FWSyntaxError $ show pat
parseSExpr e@(SList (SSym "match" : _)) = Left $ FWSyntaxError ("Malformed use of match: " ++ show e)
parseSExpr (SList [SSym "\\", SList a, b]) = do
  a' <- traverse argParser a
  b' <- parseSExpr b
  Right $ AnonFnExt a' b'
parseSExpr e@(SList (SSym "\\" : _)) = Left $ FWSyntaxError ("Malformed use of \\: " ++ show e)
parseSExpr (SList [SSym "fn", SSym s, SList a, b]) = do
  a' <- traverse argParser a
  b' <- parseSExpr b
  Right $ NamedFnExt s a' b'
parseSExpr e@(SList (SSym "fn" : _)) = Left $ FWSyntaxError ("Malformed use of fn: " ++ show e)
parseSExpr (SList (x:xs)) = do
  x' <- parseSExpr x
  xs' <- traverse parseSExpr xs
  Right $ AppExt x' xs'
parseSExpr sexpr = Left $ FWSyntaxError $ show sexpr

argParser :: SExpr -> Either FWError String
argParser sexpr = do
  s <- parseSExpr sexpr
  case s of
    IdExt s' -> Right s'
    _ -> Left $ FWSyntaxError ("Anonymous lambda argument needs to be string; found " ++ show sexpr)
