module Interpreting.Interpreter
    ( interpret
    ) where

import qualified Data.Map                     as Map
import           Errors
import           Interpreting.StandardLibrary (standardLibraryEnvironment)
import           Language                     (Environment, ExprC (..),
                                               NativeFunction (..), Value (..))

interpret :: ExprC -> Either FWError Value
interpret expr = treewalk expr (standardLibraryEnvironment strict) >>= strict

treewalk :: ExprC -> Environment -> Either FWError Value
treewalk (NumC n) _ = Right $ NumV n
treewalk NilC _ = Right NilV
treewalk TrueC _ = Right $ BoolV True
treewalk FalseC _ = Right $ BoolV False
treewalk (IdC s) env = bindResolve s env
treewalk (NegC e) env = do
  v <- treewalk e env >>= strict
  case v of
    (NumV e') -> Right $ NumV $ -e'
    _         -> Left $ FWInterpError "Negative requires a number"
treewalk (ConsC l r) env = Right $ ConsV (ThunkV l env) (ThunkV r env)
treewalk (IfC c t f) env = do
  v <- treewalk c env >>= strict
  case v of
    (BoolV True)  -> treewalk t env
    (BoolV False) -> treewalk f env
    _ -> Left $ FWInterpError "If-statement condition must evaluate to a boolean"
treewalk fn@(LambdaC _ _) env = Right $ FunctionV fn env
treewalk (AppC a b) env = do
  f <- treewalk a env >>= strict
  case f of
    (FunctionV (LambdaC a' b') clos) -> do
      b'' <- arg b
      case (a', b'') of
        (Nothing, Nothing)    -> treewalk b' clos
        (Just a'', Just b''') -> treewalk b' $ Map.insert a'' b''' clos
        _                     -> Left $ FWInterpError "Arity mismatch in function application"
    x                                -> Left $ FWInterpError ("Cannot invoke as function: " ++ (show x))
  where
    arg :: Maybe ExprC -> Either FWError (Maybe Value)
    arg Nothing    = Right Nothing
    arg (Just b'') = Right $ Just $ ThunkV b'' env
treewalk (NativeC (EnvNativeFunction f)) env = f env

bindResolve :: String -> Environment -> Either FWError Value
bindResolve s env = case Map.lookup s env of
  Nothing -> Left $ FWInterpError ("Free variable: " ++ s)
  Just v  -> Right v

strict :: Value -> Either FWError Value
strict (ThunkV e env) = treewalk e env >>= strict
strict v              = Right $ v
