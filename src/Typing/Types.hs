module Typing.Types
    ( Type (..)
    , TypeEq (..)
    , TypeConstructor (..)
    , TypeEnvironment (..)) where

import Data.Map (Map)

data Type = TypeVar Integer | TypeCon TypeConstructor [Type] deriving (Eq, Show)

data TypeEq = TEq Type Type deriving (Eq, Show)

data TypeConstructor = TCNum | TCBool | TCLambda | TCList deriving (Eq, Show)

type TypeEnvironment = Map String Type
