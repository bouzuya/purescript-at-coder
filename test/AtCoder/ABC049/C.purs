module Test.AtCoder.ABC049.C
  ( tests
  ) where

import Prelude

import AtCoder.ABC049.C as ABC049C
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC049.C" do
  TestUnit.test "example 1" do
    Assert.equal "YES" (ABC049C.solve "erasedream\n")

  TestUnit.test "example 2" do
    Assert.equal "YES" (ABC049C.solve "dreameraser\n")

  TestUnit.test "example 3" do
    Assert.equal "NO" (ABC049C.solve "dreamerer\n")
