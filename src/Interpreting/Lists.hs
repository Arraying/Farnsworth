module Interpreting.Lists
    ( curriedCons
    , force
    , head'
    , isList
    , isNil
    , tail'
    ) where

import           Errors   (FWError (..))
import           Language (Value (..), Stricter)

force :: Stricter -> Value -> Either FWError Value
force s (ConsV l r) = do
  l' <- s l >>= force s
  r' <- s r >>= force s
  Right $ ConsV l' r'
force _ v           = Right v

head' :: Value -> Either FWError Value
head' (ConsV l _) = Right l
head' NilV        = Left $ FWInterpError "Cannot determined head of Nil"
head' _           = Left $ FWInterpError "Head requires a list"

tail' :: Value -> Either FWError Value
tail' (ConsV _ r) = Right r
tail' NilV        = Left $ FWInterpError "Cannot determined head of Nil"
tail' _           = Left $ FWInterpError "Tail requires a list"

isNil :: Value -> Either FWError Value
isNil NilV        = Right $ BoolV True
isNil (ConsV _ _) = Right $ BoolV False
isNil _           = Left $ FWInterpError "List nil check requires a list"

isList :: Value -> Either FWError Value
isList NilV        = Right $ BoolV True
isList (ConsV _ _) = Right $ BoolV True
isList _           = Right $ BoolV False

curriedCons :: Value -> Value -> Either FWError Value
curriedCons l r = Right $ ConsV l r
