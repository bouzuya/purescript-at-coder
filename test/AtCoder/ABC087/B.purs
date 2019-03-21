module Test.AtCoder.ABC087.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC087.B as ABC087B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC087.B" do
  TestUnit.test "example 1" do
    Assert.equal "2" (ABC087B.solve "2\n2\n2\n100\n")

  TestUnit.test "example 2" do
    Assert.equal "0" (ABC087B.solve "5\n1\n0\n150\n")

  TestUnit.test "example 3" do
    Assert.equal "213" (ABC087B.solve "30\n40\n50\n6000\n")
