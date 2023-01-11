module Interpreting.Interpreter
    ( interpret
    ) where

import           Common
import qualified Data.Map                     as Map
import           Errors
import           Interpreting.StandardLibrary (standardLibraryEnvironment)
import           Language                     (Environment, ExprC (..),
                                               NativeFunction (..), Value (..))

interpret :: ExprC -> Either FWError Value
interpret expr = treewalk expr standardLibraryEnvironment

treewalk :: ExprC -> Environment -> Either FWError Value
treewalk (NumC n) _ = Right $ NumV n
treewalk NilC _ = Right NilV
treewalk TrueC _ = Right $ BoolV True
treewalk FalseC _ = Right $ BoolV False
treewalk (IdC s) env = bindResolve s env
treewalk (NegC e) env = mapRight neg' $ treewalk e env
  where
    neg' (NumV e') = Right $ NumV $ -e'
    neg' _         = Left $ FWInterpError "Negative requires a number"
treewalk (NandC l r) env = binOp nand' l r env
  where
    nand' (BoolV True) (BoolV True) = Right $ BoolV False
    nand' (BoolV _) (BoolV _) = Right $ BoolV True
    nand' _ _ = Left $ FWInterpError "NAND requires LHS and RHS to be booleans"
treewalk (EqC l r) env = binOp eq l r env
  where
    eq (NumV l') (NumV r') = Right $ BoolV $ l' == r'
    eq _ _ = Left $ FWInterpError "Equals requires LHS and RHS to be numbers"
treewalk (LtC l r) env = binOp lt l r env
  where
    lt (NumV l') (NumV r') = Right $ BoolV $ l' < r'
    lt _ _ = Left $ FWInterpError "Less than requires LHS and RHS to be numbers"
treewalk (GtC l r) env  = binOp gt l r env
  where
    gt (NumV l') (NumV r') = Right $ BoolV $ l' > r'
    gt _ _ = Left $ FWInterpError "Greater than requires LHS and RHS to be numbers"
treewalk (ConsC l r) env = binOp (\l' r' -> Right $ ConsV l' r') l r env
treewalk (IfC c t f) env = mapRight if' $ treewalk c env
  where
    if' (BoolV True) = treewalk t env
    if' (BoolV False) = treewalk f env
    if' _ = Left $ FWInterpError "If-statement condition must evaluate to a boolean"
treewalk fn@(LambdaC _ _) env = Right $ FunctionV fn env
treewalk (AppC f v) env = mapRight(\f' -> app f') $ treewalk f env
  where
    app :: Value -> Either FWError Value
    app (FunctionV (LambdaC a b) clos) = mapRight (\a' -> case (a, a') of
      (Nothing, Nothing) -> treewalk b clos
      (Just s, Just v') -> treewalk b $ Map.insert s v' clos
      _ -> Left $ FWInterpError "Arity mismatch in function application") $ prepApp v
    app x = Left $ FWInterpError ("Cannot invoke as function: " ++ (show x))
    prepApp :: Maybe ExprC -> Either FWError (Maybe Value)
    prepApp Nothing  = Right Nothing
    prepApp (Just a) = mapRight (\a' -> Right $ Just a') $ treewalk a env
treewalk (NativeC (EnvNativeFunction f)) env = f env


bindResolve :: String -> Environment -> Either FWError Value
bindResolve s env = case Map.lookup s env of
  Nothing -> Left $ FWInterpError ("Free variable: " ++ s)
  Just v  -> Right v

unOp :: (Value -> Either FWError Value) -> ExprC -> Environment -> Either FWError Value
unOp f expr env = mapRight f $ treewalk expr env

binOp :: (Value -> Value -> Either FWError Value) -> ExprC -> ExprC -> Environment -> Either FWError Value
binOp f l r env = mapRight (\l' -> mapRight (\r' -> f l' r') $ treewalk r env) $ treewalk l env
