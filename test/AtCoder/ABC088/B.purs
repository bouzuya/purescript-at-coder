module Test.AtCoder.ABC088.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC088.B as ABC088B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC088.B" do
  TestUnit.test "example 1" do
    Assert.equal "2" (ABC088B.solve "2\n3 1\n")

  TestUnit.test "example 2" do
    Assert.equal "5" (ABC088B.solve "3\n2 7 4\n")

  TestUnit.test "example 3" do
    Assert.equal "18" (ABC088B.solve "4\n20 18 2 18\n")
