-- https://atcoder.jp/contests/abc121/tasks/abc121_c
module Test.AtCoder.ABC121.C
  ( tests
  ) where

import Prelude

import AtCoder.ABC121.C as ABC121C
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC121.C" do
  TestUnit.test "example 1" do
    Assert.equal "12" (ABC121C.solve "2 5\n4 9\n2 4\n")

  TestUnit.test "example 2" do
    Assert.equal "130" (ABC121C.solve "4 30\n6 18\n2 5\n3 10\n7 9\n")

  TestUnit.test "example 3" do
    Assert.equal
      "100000000000000"
      (ABC121C.solve "1 100000\n1000000000 100000\n")
