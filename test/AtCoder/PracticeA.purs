module Test.AtCoder.PracticeA
  ( tests
  ) where

import Prelude

import AtCoder.PracticeA as PracticeA
import Data.Array as Array
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "PracticeA" do
  TestUnit.test "1 2 3 test" do
    Assert.equal
      "6 test"
      (PracticeA.solve "1\n2 3\ntest\n")

  TestUnit.test "1000 1000 1000 a*100" do
    let a100 = Array.fold (Array.replicate 100 "a")
    Assert.equal
      ("3000 " <> a100)
      (PracticeA.solve ("1000\n1000 1000\n" <> a100 <> "\n"))
