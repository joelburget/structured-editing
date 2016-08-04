module Test.Main where

import Prelude
import Data.Maybe
import Data.Tuple
import Operate
import Path
import Syntax
import Data.List as List
import Test.Unit.Assert as Assert
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.List (List(..))
import Test.Template (mkTemplateSuite, templateSuite)
import Test.Unit (Test, suite, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


onePlusOne :: LangSyntax
onePlusOne = Internal Addition [Leaf (IntLeaf 1), Leaf (IntLeaf 1)]

onePlusUnderscore :: LangSyntax
onePlusUnderscore = Internal Addition [Leaf (IntLeaf 1), Hole "_"]

oneTwoThree :: LangSyntax
oneTwoThree = Internal Addition
  [ Leaf (IntLeaf 1)
  , Internal Addition [Leaf (IntLeaf 2), Leaf (IntLeaf 3)]
  ]

newtype ZipperEq a b = ZipperEq (SyntaxZipper a b)
instance eqZipperEq :: (Eq a, Generic a, Generic b) => Eq (ZipperEq a b) where
  eq (ZipperEq z1) (ZipperEq z2) = z1.syntax == z2.syntax
    && pastEq z1.past z2.past
    && z1.anchor == z2.anchor
    && z1.focus == z2.focus
instance showZipperEq :: Show (ZipperEq a b) where
  show _ = "ZipperEq TODO"

pastEq :: forall a b. (Eq a, Generic a, Generic b) => Past a b -> Past a b -> Boolean
pastEq past1 past2 = case Tuple (List.uncons past1) (List.uncons past2) of
  Tuple (Just {head: h1, tail: t1}) (Just {head: h2, tail: t2}) ->
    h1.value == h2.value &&
    h1.otherChildren == h2.otherChildren &&
    h1.dir == h2.dir &&
    pastEq t1 t2
  Tuple Nothing Nothing -> true
  _ -> false

assertZEq :: forall a b e. (Eq a, Generic a, Generic b)
          => SyntaxZipper a b
          -> Either String (SyntaxZipper a b)
          -> Test e
assertZEq expected found = Assert.equal
  (Right (ZipperEq expected))
  (ZipperEq `rmap` found)

traversalsSuite = suite "traversals" do
  test "|(1 + 1)" do
    Assert.equal (Right (PathOffset 0)) (makePath onePlusOne 0)

  -- touching a number on either side counts as part of the number
  test "(|1 + 1)" do
    Assert.equal (Right (PathCons 0 (PathOffset 0))) (makePath onePlusOne 1)
  test "(1| + 1)" do
    Assert.equal (Right (PathCons 0 (PathOffset 1))) (makePath onePlusOne 2)

  test "(1 |+ 1)" do
    Assert.equal (Right (PathOffset 2)) (makePath onePlusOne 3)
  test "(1 +| 1)" do
    Assert.equal (Right (PathOffset 3)) (makePath onePlusOne 4)

  test "(1 + |1)" do
    Assert.equal (Right (PathCons 1 (PathOffset 0))) (makePath onePlusOne 5)
  test "(1 + 1|)" do
    Assert.equal (Right (PathCons 1 (PathOffset 1))) (makePath onePlusOne 6)
  test "(1 + 1)|" do
    Assert.equal (Right (PathOffset 5)) (makePath onePlusOne 7)

  test "(1 + |_)" do
    Assert.equal (Right (PathCons 1 (PathOffset 0))) (makePath onePlusOne 5)
  test "(1 + _|)" do
    Assert.equal (Right (PathCons 1 (PathOffset 1))) (makePath onePlusOne 6)

  test "(1 + |(2 + 3))" do
    Assert.equal (Right (PathOffset 4)) (makePath oneTwoThree 5)
  test "(1 + (|2 + 3))" do
    Assert.equal (Right (PathCons 1 (PathCons 0 (PathOffset 0)))) (makePath oneTwoThree 6)
  test "(1 + (2 + 3)|)" do
    Assert.equal (Right (PathOffset 4)) (makePath oneTwoThree 12)

operationsSuite = suite "operations" do
  test "inserting at front" do
    assertZEq
      { syntax: Leaf (IntLeaf 21)
      , anchor: PathOffset 1
      , focus: PathOffset 1
      , past: Nil
      }
      (operateAtomic (makeZipper (Leaf (IntLeaf 1))) (Typing '2'))

    assertZEq
      { syntax: Hole "ba"
      , anchor: PathOffset 1
      , focus: PathOffset 1
      , past: Nil
      }
      (operateAtomic (makeZipper (Hole "a")) (Typing 'b'))

  test "inserting in middle" do
    assertZEq
      { syntax: Leaf (IntLeaf 1234)
      , anchor: PathOffset 3
      , focus: PathOffset 3
      , past: Nil
      }
      (operateAtomic (makeZipper (Leaf (IntLeaf 124))) (Typing '3'))

    assertZEq
      { syntax: Hole "abcd"
      , anchor: PathOffset 2
      , focus: PathOffset 2
      , past: Nil
      }
      (operateAtomic (makeZipper (Hole "acd")) (Typing 'b'))

  test "backspacing in middle" do
    assertZEq
      { syntax: Hole "acd"
      , anchor: PathOffset 1
      , focus: PathOffset 1
      , past: Nil
      }
      (operateAtomic (makeZipper (Hole "abcd")) Backspace)

    assertZEq
      { syntax: Leaf (IntLeaf 134)
      , anchor: PathOffset 1
      , focus: PathOffset 1
      , past: Nil
      }
      (operateAtomic (makeZipper (Leaf (IntLeaf 1234))) Backspace)

  test "backspacing negative number" do
    assertZEq
      { syntax: Leaf (IntLeaf 1)
      , anchor: PathOffset 0
      , focus: PathOffset 0
      , past: Nil
      }
      (operateAtomic (makeZipper (Leaf (IntLeaf (-1)))) Backspace)


main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  traversalsSuite
  operationsSuite
  templateSuite
  mkTemplateSuite
