-- https://atcoder.jp/contests/abc121/tasks/abc121_b
module Test.AtCoder.ABC121.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC121.B as ABC121B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC121.B" do
  TestUnit.test "example 1" do
    Assert.equal "1" (ABC121B.solve "2 3 -10\n1 2 3\n3 2 1\n1 2 2\n")

  TestUnit.test "example 2" do
    Assert.equal
      "2"
      (ABC121B.solve "5 2 -4\n-2 5\n100 41\n100 40\n-3 0\n-6 -2\n18 -13\n")

  TestUnit.test "example 3" do
    Assert.equal
      "0"
      (ABC121B.solve
        "3 3 0\n100 -100 0\n0 100 100\n100 100 100\n-100 100 100\n")
