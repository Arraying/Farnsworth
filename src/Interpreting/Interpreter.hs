module Interpreting.Interpreter
    ( interpret
    , interpretStdLib
    ) where

import qualified Data.Map                     as Map
import           Errors                       (FWError (..))
import           Interpreting.StandardLibrary (standardLibraryEnvironment)
import           Language                     (Environment, ExprC (..),
                                               NativeFunction (..), Pat (..),
                                               Value (..))

interpret :: ExprC -> Either FWError Value
interpret expr = interpretStdLib expr >>= strict

interpretStdLib :: ExprC -> Either FWError Value
interpretStdLib expr = treewalk expr (standardLibraryEnvironment strict)

treewalk :: ExprC -> Environment -> Either FWError Value
treewalk (NumC n) _ = Right $ NumV n
treewalk (CharC c) _ = Right $ CharV c
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
treewalk (MatchC u xs) env = do
  u' <- treewalk u env >>= strict
  findCase u' xs
  where
    findCase :: Value -> [(Pat, ExprC)] -> Either FWError Value
    findCase _ []            = Left $ FWInterpError "Non-exhaustive cases"
    findCase u' ((p, e):xs') = do
      m <- match env u' p
      case m of
        (Just env') -> treewalk e env'
        Nothing     -> findCase u' xs'
    match :: Environment -> Value -> Pat -> Either FWError (Maybe Environment)
    match env' u' pat = case (pat, u') of
      (NumP l, NumV r)         -> if l == r then noChange else Right Nothing
      (TrueP, BoolV True)      -> noChange
      (FalseP, BoolV False)    -> noChange
      (NilP, NilV)             -> noChange
      (IdP s, _)               -> Right $ Just $ Map.insert s u' env'
      (ConsP h t, ConsV h' t') -> do
        h'' <- strict h'
        t'' <- strict t'
        l <- match env' h'' h
        r <- match env' t'' t
        case (l, r) of
          (Just l', Just r') -> Right $ Just $ Map.union (Map.union r' l') env'
          (_, _)             -> Right $ Nothing
      (_, _)                   -> Right Nothing
      where
        noChange :: Either FWError (Maybe Environment)
        noChange = Right $ Just env'
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
    x                                -> Left $ FWInterpError ("Cannot invoke as function: " ++ show x)
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
strict v              = Right v
