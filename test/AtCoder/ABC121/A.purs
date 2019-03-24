-- https://atcoder.jp/contests/abc121/tasks/abc121_a
module Test.AtCoder.ABC121.A
  ( tests
  ) where

import Prelude

import AtCoder.ABC121.A as ABC121A
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC121.A" do
  TestUnit.test "example 1" do
    Assert.equal "1" (ABC121A.solve "3 2\n2 1\n")

  TestUnit.test "example 2" do
    Assert.equal "6" (ABC121A.solve "5 5\n2 3\n")

  TestUnit.test "example 3" do
    Assert.equal "0" (ABC121A.solve "2 4\n2 4\n")
