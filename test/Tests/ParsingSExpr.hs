module Tests.ParsingSExpr
    ( tests
    ) where

import           Parsing.SExpr (SExpr (..), parseStr)
import           Test.HUnit    (Assertable (assert), Test (..))
import           TestHelper    (FWTest (..))

tests :: Test
tests = TestList
    [ number1
    , number2
    , number3
    , char1
    , char2
    , char3
    , char4
    , char5
    , string1
    , string2
    , string3
    , string4
    , string5
    , string6
    , sym1
    , sym2
    , sym3
    , sym4
    , sym5
    , sym6
    , sym7
    , sym8
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

char1 :: Test
char1 = gw "Char 1" (SChar 'a') "'a'"

char2 :: Test
char2 = gw "Char 2" (SChar '¬') "'¬'"

char3 :: Test
char3 = gw "Char 3" (SChar '\n') "'\\n'"

char4 :: Test
char4 = bw "Char 4" "'"

char5 :: Test
char5 = bw "Char 5" "'a"

string1 :: Test
string1 = gw "String 1" (SStr "") "\"\""

string2 :: Test
string2 = gw "String 2" (SStr "hello, world!") "\"hello, world!\""

string3 :: Test
string3 = gw "String 3" (SStr "hello\tworld") "\"hello\\tworld\""

string4 :: Test
string4 = bw "String 4" "\""

string5 :: Test
string5 = bw "String 5" "\"hello"

string6 :: Test
string6 = bw "String 6" "world\""

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

sym6 :: Test
sym6 = gw "Symbol 6" (SSym "a'") "a'"

sym7 :: Test
sym7 = gw "Symbol 7" (SSym "a''") "a''"

sym8 :: Test
sym8 = gw "Symbol 8" (SSym "a'''''") "a'''''"

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
