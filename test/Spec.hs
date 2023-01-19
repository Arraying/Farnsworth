import qualified Tests.ParsingSExpr (tests)

import Test.HUnit (Test(TestList), runTestTTAndExit)

main :: IO ()
main = runTestTTAndExit $ TestList [Tests.ParsingSExpr.tests]