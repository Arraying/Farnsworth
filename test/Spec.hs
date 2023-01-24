import Test.HUnit (Test(TestList), runTestTTAndExit)
import qualified Tests.Desugaring (tests)
import qualified Tests.ParsingSExpr (tests)
import qualified Tests.ParsingExt (tests)

main :: IO ()
main = runTestTTAndExit $ TestList 
    [ Tests.Desugaring.tests
    , Tests.ParsingSExpr.tests
    , Tests.ParsingExt.tests ]