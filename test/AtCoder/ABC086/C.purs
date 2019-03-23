module Test.AtCoder.ABC086.C
  ( tests
  ) where

import Prelude

import AtCoder.ABC086.C as ABC086C
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC086.C" do
  TestUnit.test "example 1" do
    Assert.equal "Yes" (ABC086C.solve "2\n3 1 2\n6 1 1\n")

  TestUnit.test "example 2" do
    Assert.equal "No" (ABC086C.solve "1\n2 100 100\n")

  TestUnit.test "example 3" do
    Assert.equal "No" (ABC086C.solve "2\n5 1 1\n100 1 1\n")
