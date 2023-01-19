module Interpreting.StandardLibrary
    ( standardLibraryEnvironment
    ) where

import qualified Data.Map                     as Map
import           Interpreting.Arithmetic      (divide, minus, multiply, plus)
import           Interpreting.Lists           (curriedCons, force, head',
                                               isList, isNil, tail')
import           Interpreting.Logic           (and', eq, gt, gte, lt, lte, neq,
                                               not', or')
import           Interpreting.NativeFunctions (function0, function1, function1',
                                               function2)
import           Language                     (Environment, Stricter,
                                               Value (..))

standardLibraryEnvironment :: Stricter -> Environment
standardLibraryEnvironment strict = Map.fromList
    [ ("best-number", function0 $ Right $ NumV 19) -- My favourite number!
    , ("id", f1 Right)
    , ("!", f1 not')
    , ("force", f1' force)
    , ("head", f1 head')
    , ("tail", f1 tail')
    , ("nil?", f1 isNil)
    , ("list?", f1 isList)
    , ("+", f2 plus)
    , ("-", f2 minus)
    , ("*", f2 multiply)
    , ("/", f2 divide)
    , ("&&", f2 and')
    , ("||", f2 or')
    , ("==", f2 eq)
    , ("!=", f2 neq)
    , ("<", f2 lt)
    , (">", f2 gt)
    , ("<=", f2 lte)
    , (">=", f2 gte)
    , ("cons_", f2 curriedCons) ]
  where
    f1 = function1 strict
    f1' = function1' strict
    f2 = function2 strict
