module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.ABC124.D as Solver
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  Solver.tests
