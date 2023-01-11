module Desugaring.Desugarer
    ( ExprC (..)
    , desugar
    ) where

import           Common
import           Desugaring.Lambdas (curryApplication, curryLambda, namedLambda)
import           Errors
import           Language           (ExprC (..), ExprExt (..))

desugar :: ExprExt -> Either FWError ExprC
desugar (NumExt n)      = Right $ NumC n
desugar NilExt          = Right NilC
desugar TrueExt         = Right TrueC
desugar FalseExt        = Right FalseC
desugar (IdExt s)       = Right $ IdC s
desugar (UnOpExt "-" e) = unOp (\e' -> negative e') e
desugar (UnOpExt "!" e) = unOp (\e' -> not' e') e
desugar (UnOpExt "head" e) = unOp (\e' -> HeadC e') e
desugar (UnOpExt "tail" e) = unOp (\e' -> TailC e') e
desugar (UnOpExt "nil?" e) = unOp (\e' -> IsNilC e') e
desugar (UnOpExt "list?" e) = unOp (\e' -> IsListC e') e
desugar (BinOpExt "+" l r) = binOp (\(l', r') -> PlusC l' r') l r
desugar (BinOpExt "-" l r) = binOp (\(l', r') -> PlusC l' $ negative r') l r
desugar (BinOpExt "*" l r) = binOp (\(l', r') -> MultC l' r') l r
desugar (BinOpExt "/" l r) = binOp (\(l', r') -> DivC l' r') l r
desugar (BinOpExt "&&" l r) = binOp (\(l', r') -> and' l' r') l r
desugar (BinOpExt "||" l r) = binOp (\(l', r') -> or' l' r') l r
desugar (BinOpExt "==" l r) = binOp (\(l', r') -> EqC l' r') l r
desugar (BinOpExt "!=" l r) = binOp (\(l', r') -> not' $ EqC l' r') l r
desugar (BinOpExt "<" l r) = binOp (\(l', r') ->  LtC l' r') l r
desugar (BinOpExt ">" l r) = binOp (\(l', r') -> GtC l' r') l r
desugar (BinOpExt "<=" l r) = binOp (\(l', r') -> leq' l' r') l r
desugar (BinOpExt ">=" l r) = binOp (\(l', r') -> geq' l' r') l r
desugar (BinOpExt "cons" l r) = binOp (\(l', r') -> ConsC l' r') l r
desugar (IfExt c t f) = case mapMany desugar [c, t, f] of
  Left e             -> Left e
  Right [c', t', f'] -> Right $ IfC c' t' f'
  Right _            -> Left $ FWDesugError "Internal if-statement error"
desugar (ListExt xs) = mapRight (\xs' -> Right $ list xs') $ mapMany desugar xs
desugar (AnonFnExt a b) = mapRight (\b' -> Right $ curryLambda a b') $ desugar b
desugar (NamedFnExt s a b) = mapRight (\b' -> namedLambda s $ curryLambda a b') $ desugar b
desugar (AppExt x xs) = mapRight (\x' -> mapRight (\xs' -> Right $ curryApplication x' xs') $ mapMany desugar xs) $ desugar x
desugar e               = Left $ FWDesugError $ show e

unOp :: (ExprC -> ExprC) -> ExprExt -> Either FWError ExprC
unOp f e = mapRight (\e' -> Right $ f e') $ desugar e

binOp :: ((ExprC, ExprC) -> ExprC) -> ExprExt -> ExprExt -> Either FWError ExprC
binOp f l r = mapRight (\t -> Right $ f t) $ mapBin desugar (l, r)

list :: [ExprC] -> ExprC
list []     = NilC
list (x:xs) = ConsC x $ list xs

negative :: ExprC -> ExprC
negative e = MultC (NumC (-1)) e

not' :: ExprC -> ExprC
not' e = AppC (LambdaC (Just "e") (NandC (IdC "e") (IdC "e"))) $ Just e

and' :: ExprC -> ExprC -> ExprC
and' l r = curryApplication (curryLambda ["l", "r"] (NandC (NandC (IdC "l") (IdC "r")) (NandC (IdC "l") (IdC "r")))) [l, r]

or' :: ExprC -> ExprC -> ExprC
or' l r = curryApplication (curryLambda ["l", "r"] (NandC (NandC (IdC "l") (IdC "l")) (NandC (IdC "r") (IdC "r")))) [l, r]

leq' :: ExprC -> ExprC -> ExprC
leq' l r = curryApplication (curryLambda ["l", "r"] $ or' (LtC (IdC "l") (IdC "r")) (EqC (IdC "l") (IdC "r"))) [l, r]

geq' :: ExprC -> ExprC -> ExprC
geq' l r = curryApplication (curryLambda ["l", "r"] $ or' (GtC (IdC "l") (IdC "r")) (EqC (IdC "l") (IdC "r"))) [l, r]


