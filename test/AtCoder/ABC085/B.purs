module Test.AtCoder.ABC085.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC085.B as ABC085B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC085.B" do
  TestUnit.test "example 1" do
    Assert.equal "3" (ABC085B.solve "4\n10\n8\n8\n6\n")

  TestUnit.test "example 2" do
    Assert.equal "1" (ABC085B.solve "3\n15\n15\n15\n")

  TestUnit.test "example 3" do
    Assert.equal "4" (ABC085B.solve "7\n50\n30\n50\n100\n50\n80\n30\n")
