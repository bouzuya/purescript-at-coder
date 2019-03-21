module Test.AtCoder.ABC083.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC083.B as ABC083B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC083.B" do
  TestUnit.test "example 1" do
    Assert.equal "84" (ABC083B.solve "20 2 5\n")

  TestUnit.test "example 2" do
    Assert.equal "13" (ABC083B.solve "10 1 2\n")

  TestUnit.test "example 3" do
    Assert.equal "4554" (ABC083B.solve "100 4 16\n")
