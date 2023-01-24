module Tests.Desugaring
    ( tests
    ) where

import           Desugaring.Desugarer (desugar)
import           Language             (ExprC (..), ExprExt (..), Pat (..))
import           Test.HUnit           (Assertable (assert), Test (..))
import           TestHelper           (FWTest (..))

tests :: Test
tests = TestList
  [ number
  , char
  , string
  , ident
  , boolean1
  , boolean2
  , negative1
  , negative2
  , cons1
  , cons2
  , cons3
  , cons4
  , cons5
  , if1
  , if2
  , match
  , lambda1
  , lambda2
  , lambda3
  , app1
  , app2
  , app3 ]

number :: Test
number = gw "Number" (NumC 1) (NumExt 1)

char :: Test
char = gw "Char" (CharC 'a') (CharExt 'a')

string :: Test
string = gw "String" (ConsC (CharC 'a') $ ConsC (CharC 'b') $ ConsC (CharC 'c') NilC) (StrExt "abc")

ident :: Test
ident = gw "Identifier" (IdC "fooBar") (IdExt "fooBar")

boolean1 :: Test
boolean1 = gw "Boolean 1" TrueC TrueExt

boolean2 :: Test
boolean2 = gw "Boolean 2" FalseC FalseExt

negative1 :: Test
negative1 = gw "Negative 1" (NegC (NumC 1)) (UnOpExt "-" (NumExt 1))

negative2 :: Test
negative2 = gw "Negative 2" (NegC (NegC (NumC 19))) (UnOpExt "-" (UnOpExt "-" (NumExt 19)))

cons1 :: Test
cons1 = gw "Cons 1" (ConsC (NumC 1) NilC) (BinOpExt "cons" (NumExt 1) NilExt)

cons2 :: Test
cons2 = gw "Cons 2" (ConsC (NumC 2) (ConsC (NumC 3) NilC)) (BinOpExt "cons" (NumExt 2) (BinOpExt "cons" (NumExt 3) NilExt))

cons3 :: Test
cons3 = gw "Cons 3" NilC (ListExt [])

cons4 :: Test
cons4 = gw "Cons 4" (ConsC (NumC 1) NilC) (ListExt [NumExt 1])

cons5 :: Test
cons5 = gw "Cons 5" (ConsC (NumC 2) (ConsC (NumC 3) NilC)) (ListExt [NumExt 2, NumExt 3])

if1 :: Test
if1 = gw "If 1" (IfC TrueC NilC (ConsC (NumC 1) NilC)) (IfExt TrueExt NilExt (ListExt [NumExt 1]))

if2 :: Test
if2 = gw "If 2" (IfC TrueC (NumC 1) (NumC 2)) (IfExt TrueExt (NumExt 1) (NumExt 2))

match :: Test
match = gw "Match" (MatchC NilC [(NilP, NilC), (ConsP (IdP "h") (IdP "t"), NilC)]) (MatchExt NilExt [(NilP, NilExt), (ConsP (IdP "h") (IdP "t"), NilExt)])

lambda1 :: Test
lambda1 = gw "Lambda 1" (LambdaC Nothing NilC) (AnonFnExt [] NilExt)

lambda2 :: Test
lambda2 = gw "Lambda 2" (LambdaC (Just "x") (IdC "x")) (AnonFnExt ["x"] (IdExt "x"))

lambda3 :: Test
lambda3 = gw "Lambda 3" (LambdaC (Just "x") (LambdaC (Just "y") NilC)) (AnonFnExt ["x", "y"] NilExt)

app1 :: Test
app1 = gw "App 1" (AppC (IdC "f") Nothing) (AppExt (IdExt "f") [])

app2 :: Test
app2 = gw "App 2" (AppC (IdC "f") (Just $ NumC 1)) (AppExt (IdExt "f") [NumExt 1])

app3 :: Test
app3 = gw "App 3" (AppC (AppC (IdC "f") $ Just $ NumC 1) (Just $ NumC 2)) (AppExt (IdExt "f") [NumExt 1, NumExt 2])

gw :: String -> ExprC -> ExprExt -> Test
gw label e ex = TestLabel label $ TestCase $ assert $ Pass e $ desugar ex
