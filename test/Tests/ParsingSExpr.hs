module Tests.ParsingSExpr (tests) where

import Parsing.SExpr (SExpr(..), parseStr)
import Test.HUnit (Assertable(assert), Test(..))
import TestHelper (FWTest(..))

tests :: Test
tests = TestList
    [ number1
    , number2
    , number3
    , sym1
    , sym2
    , sym3
    , sym4
    , sym5
    , list1
    , list2
    , list3
    , list4
    , list5
    , list6 ]

number1 :: Test
number1 = gw "Number 1" (SNum 1) "1"

number2 :: Test
number2 = gw "Number 2" (SNum 234) "234"

number3 :: Test
number3 = bw "Number 3" "1two3"

sym1 :: Test
sym1 = gw "Symbol 1" (SSym "foo") "foo"

sym2 :: Test
sym2 = gw "Symbol 2" (SSym "f123") "f123"

sym3 :: Test
sym3 = gw "Symbol 3" (SSym "%^&*-_=+;:") "%^&*-_=+;:"

sym4 :: Test
sym4 = bw "Symbol 4" "1foobar"

sym5 :: Test
sym5 = gw "Symbol 5" (SSym "-1") "-1" -- Cursed, I know.

list1 :: Test
list1 = gw "List 1" (SList []) "()"

list2 :: Test
list2 = gw "List 2" (SList [SNum 19]) "(19)"

list3 :: Test
list3 = gw "List 3" (SList [SSym "list", SNum 1, SNum 2, SNum 3]) "(list 1 2 3)"

list4 :: Test
list4 = gw "List 4" (SList [SList [SSym "+", SNum 1], SNum 1]) "((+ 1) 1)"

list5 :: Test
list5 = bw "List 5" "(foo"

list6 :: Test
list6 = bw "List 6" "bar)"

gw :: String -> SExpr -> String -> Test
gw label s str = TestLabel label $ TestCase $ assert $ Pass s $ parseStr str

bw :: String -> String -> Test
bw label str = TestLabel label $ TestCase $ assert $ Fail $ parseStr str
