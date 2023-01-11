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
desugar (UnOpExt "-" e) = mapRight (\e' -> Right $ NegC e') $ desugar e
desugar (BinOpExt "cons" l r) = mapRight (\(l', r') -> Right $ ConsC l' r') $ mapBin desugar (l, r)
desugar (IfExt c t f) = case mapMany desugar [c, t, f] of
  Left e             -> Left e
  Right [c', t', f'] -> Right $ IfC c' t' f'
  Right _            -> Left $ FWDesugError "Internal if-statement error"
desugar (ListExt xs) = mapRight (\xs' -> Right $ list xs') $ mapMany desugar xs
desugar (AnonFnExt a b) = mapRight (\b' -> Right $ curryLambda a b') $ desugar b
desugar (NamedFnExt s a b) = mapRight (\b' -> namedLambda s $ curryLambda a b') $ desugar b
desugar (AppExt x xs) = mapRight (\x' -> mapRight (\xs' -> Right $ curryApplication x' xs') $ mapMany desugar xs) $ desugar x
desugar e               = Left $ FWDesugError $ show e

list :: [ExprC] -> ExprC
list []     = NilC
list (x:xs) = ConsC x $ list xs
