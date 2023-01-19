module TestHelper
    ( FWTest (..)
    ) where

import           Errors     (FWError)
import           Test.HUnit (Assertable (assert), assertBool, assertEqual)

data FWTest a
  = Pass a (Either FWError a)
  | Fail (Either FWError a)

instance (Eq a, Show a) => Assertable (FWTest a) where
  assert (Pass _ (Left v)) = assertBool ("Expected a pass, but found failure: " ++ show v) False
  assert (Pass e (Right a)) = assertEqual "Good-weather assertion: " e a
  assert (Fail (Left _)) = assertBool "Expected a failure, and got one: " True
  assert (Fail (Right a)) = assertBool ("Expected a failure, but found success: " ++ show a) False
