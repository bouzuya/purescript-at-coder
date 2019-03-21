module Test.AtCoder.ABC081.B
  ( tests
  ) where

import Prelude

import AtCoder.ABC081.B as ABC081B
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC081.B" do
  TestUnit.test "example 1" do
    Assert.equal "2" (ABC081B.solve "3\n8 12 40\n")

  TestUnit.test "example 2" do
    Assert.equal "0" (ABC081B.solve "4\n5 6 8 10\n")

  TestUnit.test "example 3" do
    Assert.equal
      "8"
      (ABC081B.solve
        "6\n382253568 723152896 37802240 379425024 404894720 471526144\n")
