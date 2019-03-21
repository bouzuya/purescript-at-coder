module Test.AtCoder.ABC086.A
  ( tests
  ) where

import Prelude

import AtCoder.ABC086.A as ABC086A
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC086.A" do
  TestUnit.test "3 4" do
    Assert.equal "Even" (ABC086A.solve "3 4\n")

  TestUnit.test "1 21" do
    Assert.equal "Odd" (ABC086A.solve "1 21\n")
