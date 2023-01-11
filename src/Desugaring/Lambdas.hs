module Desugaring.Lambdas
    ( curryApplication
    , curryLambda
    , namedLambda
    ) where

import           Errors   (FWError (..))
import           Language (ExprC (..))

curryLambda :: [String] -> ExprC -> ExprC
curryLambda [] b     = LambdaC Nothing b
curryLambda [x] b    = LambdaC (Just x) b
curryLambda (x:xs) b = LambdaC (Just x) $ curryLambda xs b

curryApplication :: ExprC -> [ExprC] -> ExprC
curryApplication f []     = AppC f Nothing
curryApplication f [x]    = AppC f $ Just x
curryApplication f (x:xs) = curryApplication (AppC f $ Just x) xs

namedLambda :: String -> ExprC -> Either FWError ExprC
namedLambda name (LambdaC a b) = Right $ AppC zCombinator $ Just $ LambdaC (Just name) $ LambdaC a b
namedLambda _ _ = Left $ FWDesugError "Cannot z-combinate non lambda"

-- const Z = g => (x => g(v => x(x)(v)))(x => g(v => x(x)(v)))
zCombinator :: ExprC
zCombinator = LambdaC (Just "f") $ AppC innerFn (Just innerFn)
  where
    innerFn :: ExprC
    innerFn = LambdaC (Just "x") $ AppC (IdC "f") (Just embeddedFn)
    embeddedFn :: ExprC
    embeddedFn = LambdaC (Just "v") (AppC (AppC (IdC "x") (Just $ IdC "x")) (Just $ IdC "v"))
