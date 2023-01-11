module Interpreting.StandardLibrary
    ( standardLibraryEnvironment
    ) where

import qualified Data.Map                     as Map
import           Interpreting.Arithmetic
import           Interpreting.Lists
import           Interpreting.Logic
import           Interpreting.NativeFunctions (function0, function1, function2)
import           Language                     (Environment, Value (..))

standardLibraryEnvironment :: Environment
standardLibraryEnvironment = Map.fromList
    [ ("best-number", function0 $ Right $ NumV 19) -- My favourite number!
    , ("id", function1 (\x -> Right x))
    , ("!", function1 not')
    , ("head", function1 head')
    , ("tail", function1 tail')
    , ("nil?", function1 isNil)
    , ("list?", function1 isList)
    , ("+", function2 plus)
    , ("-", function2 minus)
    , ("*", function2 multiply)
    , ("/", function2 divide)
    , ("&&", function2 and')
    , ("||", function2 or')
    , ("==", function2 eq)
    , ("!=", function2 neq)
    , ("<", function2 lt)
    , (">", function2 gt)
    , ("<=", function2 lte)
    , (">=", function2 gte)
    , ("cons_", function2 curriedCons) ]
