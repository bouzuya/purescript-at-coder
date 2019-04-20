module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.Tenka12019Beginner.C as Solver
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  Solver.tests
