module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.ABC119.B as Solver
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  Solver.tests
