module Test.Helper
  ( tests
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Helper as Helper
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Helper" do
  TestUnit.test "binarySearch" do
    let xs = [1, 3, 5]
    Assert.equal (Maybe.Just 0) (Helper.binarySearch 1 xs)
    Assert.equal (Maybe.Just 1) (Helper.binarySearch 3 xs)
    Assert.equal (Maybe.Nothing) (Helper.binarySearch 4 xs)

  TestUnit.test "findMaxIndex" do
    Assert.equal (Maybe.Just 1) (Helper.findMaxIndex [1, 3, 2])
    Assert.equal (Maybe.Nothing) (Helper.findMaxIndex ([] :: Array Int))

  TestUnit.test "int2" do
    Assert.equal (Maybe.Just (Tuple.Tuple 1 2)) (Helper.int2 "1 2")
    Assert.equal (Maybe.Nothing) (Helper.int2 "1 2 3")
    Assert.equal (Maybe.Nothing) (Helper.int2 "1 a")

  TestUnit.test "splitByNL" do
    Assert.equal ["abc"] (Helper.splitByNL "abc\n")
    Assert.equal ["abc", "123"] (Helper.splitByNL "abc\n123\n")

  TestUnit.test "splitBySP" do
    Assert.equal ["abc"] (Helper.splitBySP "abc")
    Assert.equal ["abc", "123"] (Helper.splitBySP "abc 123")
