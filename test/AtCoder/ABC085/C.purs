module Test.AtCoder.ABC085.C
  ( tests
  ) where

import Prelude

import AtCoder.ABC085.C as ABC085C
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC085.C" do
  TestUnit.test "example 1" do
    Assert.equal "4 0 5" (ABC085C.solve "9 45000\n")

  TestUnit.test "example 2" do
    Assert.equal "-1 -1 -1" (ABC085C.solve "20 196000\n")

  TestUnit.test "example 3" do
    -- Assert.equal "14 27 959" (ABC085C.solve "1000 1234000\n")
    Assert.equal
      (Just (10000 * 14 + 5000 * 27 + 1000 * 959))
      (map
        (\{ x, y, z } -> 10000 * x + 5000 * y + 1000 * z)
        (Array.head (ABC085C.solve'' 1000 1234000)))

  TestUnit.test "example 4" do
    Assert.equal "2000 0 0" (ABC085C.solve "2000 20000000\n")
