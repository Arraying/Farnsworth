module Main
    ( main
    ) where

import           Test.HUnit         (Test (TestList), runTestTTAndExit)
import qualified Tests.Desugaring   (tests)
import qualified Tests.ParsingExt   (tests)
import qualified Tests.ParsingSExpr (tests)

main :: IO ()
main = runTestTTAndExit $ TestList
    [ Tests.Desugaring.tests
    , Tests.ParsingSExpr.tests
    , Tests.ParsingExt.tests ]
