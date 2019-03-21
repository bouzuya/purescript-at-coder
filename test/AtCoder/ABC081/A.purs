module Test.AtCoder.ABC081.A
  ( tests
  ) where

import Prelude

import AtCoder.ABC081.A as ABC081A
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC081.A" do
  TestUnit.test "101" do
    Assert.equal "2" (ABC081A.solve "101\n")

  TestUnit.test "000" do
    Assert.equal "0" (ABC081A.solve "000\n")
