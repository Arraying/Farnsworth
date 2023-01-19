module Tests.ParsingExt (tests) where

import Language (ExprExt(..), Pat(..))
import Parsing.Parser (parseSExpr)
import Parsing.SExpr (parseStr)
import Test.HUnit (Assertable(assert), Test(..))
import TestHelper (FWTest(..))

tests :: Test
tests = TestList
    [ number
    , symbol
    , boolean1
    , boolean2
    , unary1
    , unary2
    , binary1
    , binary2
    , if1
    , if2
    , if3
    , if4
    , if5
    , if6
    , list1
    , list2
    , list3
    , match1
    , match2
    , match3
    , match4
    , match5
    , match6
    , match7
    , match8
    , match9
    , match10
    , match11
    , match12
    , match13
    , match14
    , anon1
    , anon2
    , anon3
    , anon4
    , anon5
    , anon6
    , anon7
    , anon8
    , fn1
    , fn2
    , fn3
    , fn4
    , fn5
    , fn6
    , fn7
    , fn8
    , fn9
    , fn10
    , app1
    , app2 ]

number :: Test
number = gw "Number" (NumExt 1) "1"

symbol :: Test
symbol = gw "Symbol" (IdExt "+") "+"

boolean1 :: Test
boolean1 = gw "Boolean 1" TrueExt "True"

boolean2 :: Test
boolean2 = gw "Boolean 2" FalseExt "False"

unary1 :: Test
unary1 = gw "Unary 1" (UnOpExt "-" $ NumExt 19) "(- 19)"

unary2 :: Test
unary2 = gw "Unary 2" (UnOpExt "-" $ UnOpExt "-" $ NumExt 19) "(- (- 19))"

binary1 :: Test
binary1 = gw "Binary 1" (BinOpExt "cons" (NumExt 1) NilExt) "(cons 1 Nil)"

binary2 :: Test
binary2 = gw "Binary 2" (BinOpExt "cons" TrueExt $ BinOpExt "cons" FalseExt NilExt) "(cons True (cons False Nil))"

if1 :: Test
if1 = gw "If 1" (IfExt TrueExt (NumExt 1) (NumExt 2)) "(if True 1 2)"

if2 :: Test
if2 = gw "If 2" (IfExt TrueExt (IfExt FalseExt NilExt NilExt) NilExt) "(if True (if False Nil Nil) Nil)"

if3 :: Test
if3 = bw "If 3" "(if True 1)"

if4 :: Test
if4 = bw "If 4" "(if True)"

if5 :: Test
if5 = bw "If 5" "(if)"

if6 :: Test
if6 = bw "If 6" "if"

list1 :: Test
list1 = gw "List 1" (ListExt []) "(list)"

list2 :: Test
list2 = gw "List 2" (ListExt [NumExt 1, NumExt 2]) "(list 1 2)"

list3 :: Test
list3 = bw "List 3" "list"

match1 :: Test
match1 = gw "Match 1" (MatchExt (NumExt 1) [(NumP 1, NilExt), (IdP "_", NilExt)]) "(match 1 (case 1 Nil) (case _ Nil))"

match2 :: Test
match2 = gw "Match 2" (MatchExt (NumExt 1) []) "(match 1)"

match3 :: Test
match3 = bw "Match 3" "(match)"

match4 :: Test
match4 = bw "Match 4" "match"

match5 :: Test
match5 = gw "Match 5" (MatchExt TrueExt [(TrueP, NilExt)]) "(match True (case True Nil))"

match6 :: Test
match6 = gw "Match 6" (MatchExt TrueExt [(FalseP, NilExt)]) "(match True (case False Nil))"

match7 :: Test
match7 = gw "Match 7" (MatchExt TrueExt [(NilP, NilExt)]) "(match True (case Nil Nil))"

match8 :: Test
match8 = gw "Match 8" (MatchExt TrueExt [(ConsP (NumP 1) NilP, NilExt)]) "(match True (case (cons 1 Nil) Nil))"

match9 :: Test
match9 = gw "Match 9" (MatchExt TrueExt [(ConsP (NumP 1) (ConsP (NumP 2) NilP), NumExt 2)]) "(match True (case (cons 1 (cons 2 Nil)) 2))"

match10 :: Test
match10 = bw "Match 10" "(match 1 (case 1))"

match11 :: Test
match11 = bw "Match 11" "(match 1 (case))"

match12 :: Test
match12 = bw "Match 12" "(match 1 (case))"

match13 :: Test
match13 = bw "Match 13" "(match 1 (case 1 2 3))"

match14 :: Test
match14 = bw "Match 13" "(match 1 (case (list 1 2 3) 1))"

anon1 :: Test
anon1 = gw "Anon 1" (AnonFnExt [] TrueExt) "(\\ () True)"

anon2 :: Test
anon2 = gw "Anon 2" (AnonFnExt ["a"] TrueExt) "(\\ (a) True)"

anon3 :: Test
anon3 = gw "Anon 3" (AnonFnExt ["a", "b"] TrueExt) "(\\ (a b) True)"

anon4 :: Test
anon4 = bw "Anon 4" "(\\ ())"

anon5 :: Test
anon5 = bw "Anon 5" "(\\)"

anon6 :: Test
anon6 = bw "Anon 6" "\\"

anon7 :: Test
anon7 = bw "Anon 7" "(\\ a True)"

anon8 :: Test
anon8 = bw "Anon 8" "(\\ (1) True)"

fn1 :: Test
fn1 = gw "Fn 1" (NamedFnExt "foo" [] TrueExt) "(fn foo () True)"

fn2 :: Test
fn2 = gw "Fn 2" (NamedFnExt "foo" ["a"] TrueExt) "(fn foo (a) True)"

fn3 :: Test
fn3 = gw "Fn 3" (NamedFnExt "foo" ["a", "b"] TrueExt) "(fn foo (a b) True)"

fn4 :: Test
fn4 = bw "Fn 4" "(fn foo ())"

fn5 :: Test
fn5 = bw "Fn 5" "(fn foo)"

fn6 :: Test
fn6 = bw "Fn 6" "(fn)"

fn7 :: Test
fn7 = bw "Fn 7" "fn"

fn8 :: Test
fn8 = bw "Fn 8" "(fn foo a True)"

fn9 :: Test
fn9 = bw "Fn 9" "(fn foo (1) True)"

fn10 :: Test
fn10 = bw "Fn 10" "(fn 123 () True)"

app1 :: Test
app1 = gw "App 1" (AppExt (AnonFnExt [] FalseExt) []) "((\\ () False))"

app2 :: Test
app2 = gw "App 2" (AppExt (AnonFnExt ["a", "b"] (AppExt (IdExt "+") [IdExt "a", IdExt "b"])) [NumExt 1, NumExt 2]) "((\\ (a b) (+ a b)) 1 2)"

gw :: String -> ExprExt -> String -> Test
gw label e str = TestLabel label $ TestCase $ assert $ Pass e (parseStr str >>= parseSExpr)

bw :: String -> String -> Test
bw label str = TestLabel label $ TestCase $ assert $ Fail (parseStr str >>= parseSExpr)