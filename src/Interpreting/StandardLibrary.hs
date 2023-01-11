module Interpreting.StandardLibrary
    ( standardLibraryEnvironment
    ) where

import qualified Data.Map                     as Map
import           Errors                       (FWError (..))
import           Interpreting.Arithmetic
import Interpreting.Lists (head', tail', isNil, isList)
import           Interpreting.Logic
import           Interpreting.NativeFunctions (function0, function1, function2)
import           Language                     (Environment, ExprC (..),
                                               NativeFunction (..), Value (..))

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
    , ("&&", function2 and')
    , ("||", function2 or') ]
