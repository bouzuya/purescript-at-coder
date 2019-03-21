module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.PracticeA as PracticeA
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  PracticeA.tests
