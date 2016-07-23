module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Test.Unit (suite, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Main


onePlusOne :: Syntax
onePlusOne = Plus (SNumber 1) (SNumber 1)

onePlusUnderscore :: Syntax
onePlusUnderscore = Plus (SNumber 1) (Hole "_")

oneTwoThree :: Syntax
oneTwoThree = Plus (SNumber 1) (Plus (SNumber 2) (SNumber 3))

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  suite "traversals" do
    test "|(1 + 1)" do
      Assert.equal (Right (PathOffset 0)) (makePath onePlusOne 0)

    -- touching a number on either side counts as part of the number
    test "(|1 + 1)" do
      Assert.equal (Right (PathCons StepLeft (PathOffset 0))) (makePath onePlusOne 1)
    test "(1| + 1)" do
      Assert.equal (Right (PathCons StepLeft (PathOffset 1))) (makePath onePlusOne 2)

    test "(1 |+ 1)" do
      Assert.equal (Right (PathOffset 2)) (makePath onePlusOne 3)
    test "(1 +| 1)" do
      Assert.equal (Right (PathOffset 3)) (makePath onePlusOne 4)

    test "(1 + |1)" do
      Assert.equal (Right (PathCons StepRight (PathOffset 0))) (makePath onePlusOne 5)
    test "(1 + 1|)" do
      Assert.equal (Right (PathCons StepRight (PathOffset 1))) (makePath onePlusOne 6)
    test "(1 + 1)|" do
      Assert.equal (Right (PathOffset 5)) (makePath onePlusOne 7)

    test "(1 + |_)" do
      Assert.equal (Right (PathCons StepRight (PathOffset 0))) (makePath onePlusOne 5)
    test "(1 + _|)" do
      Assert.equal (Right (PathCons StepRight (PathOffset 1))) (makePath onePlusOne 6)

    test "(1 + |(2 + 3))" do
      Assert.equal (Right (PathOffset 4)) (makePath oneTwoThree 5)
    test "(1 + (|2 + 3))" do
      Assert.equal (Right (PathCons StepRight (PathCons StepLeft (PathOffset 0)))) (makePath oneTwoThree 6)
    test "(1 + (2 + 3)|)" do
      Assert.equal (Right (PathOffset 4)) (makePath oneTwoThree 12)
