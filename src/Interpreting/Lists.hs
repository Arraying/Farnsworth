module Interpreting.Lists
    ( head'
    , tail'
    , isNil
    , isList
    ) where

import           Errors   (FWError (..))
import           Language (Value (..))

head' :: Value -> Either FWError Value
head' (ConsV l _) = Right l
head' NilV        = Left $ FWInterpError "Cannot determined head of Nil"
head' _           = Left $ FWInterpError "Head requires a list"

tail' :: Value -> Either FWError Value
tail' (ConsV _ r) = Right r
tail' NilV        = Left $ FWInterpError "Cannot determined head of Nil"
tail' _           = Left $ FWInterpError "Tail requires a list"

isNil :: Value -> Either FWError Value
isNil NilV = Right $ BoolV True
isNil (ConsV _ _) = Right $ BoolV False
isNil _ = Left $ FWInterpError "List nil check requires a list"

isList :: Value -> Either FWError Value
isList NilV        = Right $ BoolV True
isList (ConsV _ _) = Right $ BoolV True
isList _           = Right $ BoolV False