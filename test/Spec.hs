import Test.HUnit (Test(TestList), runTestTTAndExit)
import qualified Tests.ParsingSExpr (tests)
import qualified Tests.ParsingExt (tests)

main :: IO ()
main = runTestTTAndExit $ TestList 
    [ Tests.ParsingSExpr.tests
    , Tests.ParsingExt.tests ]