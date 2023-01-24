module Desugaring.Desugarer
    ( ExprC (..)
    , desugar
    ) where

import           Desugaring.Lambdas (curryApplication, curryLambda, namedLambda)
import           Errors             (FWError (..))
import           Language           (ExprC (..), ExprExt (..), Pat)

desugar :: ExprExt -> Either FWError ExprC
desugar (NumExt n)      = Right $ NumC n
desugar (CharExt c)     = Right $ CharC c
desugar (StrExt str)    = Right $ list $ map CharC str
desugar NilExt          = Right NilC
desugar TrueExt         = Right TrueC
desugar FalseExt        = Right FalseC
desugar (IdExt s)       = Right $ IdC s
desugar (UnOpExt "-" e) = do
  e' <- desugar e
  Right $ NegC e'
desugar (BinOpExt "cons" l r) = do
  l' <- desugar l
  r' <- desugar r
  Right $ ConsC l' r'
desugar (IfExt c t f) = do
  c' <- desugar c
  t' <- desugar t
  f' <- desugar f
  Right $ IfC c' t' f'
desugar (ListExt xs) = do
  xs' <- traverse desugar xs
  Right $ list xs'
desugar (MatchExt x xs) = do
  x' <- desugar x
  xs' <- traverse desugarCase xs
  Right $ MatchC x' xs'
  where
    desugarCase :: (Pat, ExprExt) -> Either FWError (Pat, ExprC)
    desugarCase (p, e) = do
      e' <- desugar e
      Right $ (p, e')
desugar (AnonFnExt a b) = do
  b' <- desugar b
  Right $ curryLambda a b'
desugar (NamedFnExt s a b) = do
  c <- curryLambda a <$> desugar b
  namedLambda s c
desugar (AppExt x xs) = do
  x' <- desugar x
  xs' <- traverse desugar xs
  Right $ curryApplication x' xs'
desugar e               = Left $ FWDesugError $ show e

list :: [ExprC] -> ExprC
list = foldr ConsC NilC
