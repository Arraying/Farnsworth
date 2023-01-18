module Typing.TypeChecker where

import qualified Data.Map as Map
import Errors (FWError(..))
import Language (ExprC(..))
import Typing.Types (Type(..), TypeEq(..), TypeConstructor(..), TypeEnvironment)


type' :: ExprC -> Either FWError Type
type' e = typeOf e Map.empty

typeOf :: ExprC -> TypeEnvironment -> Either FWError Type
typeOf e env = do
  let t = TypeVar 0 -- 0 reserved for root type.
  let cs = generate e t env nats 
  subs <- unify cs []
  Right $ lookup' subs t

generate :: ExprC -> Type -> TypeEnvironment -> [Integer] -> [TypeEq]
generate e t env ids = []

unify :: [TypeEq] -> [TypeEq] -> Either FWError [TypeEq]
unify cs sub = Right []

lookup' :: [TypeEq] -> Type -> Type
lookup' [] t = t
lookup' ((TEq l r):xs) t = if (l == t) then r else lookup' xs t

tvar :: [Integer] -> (Type, [Integer])
tvar (x:xs) = (TypeVar x, xs)

nats :: [Integer]
nats = 1 : map (+1) nats