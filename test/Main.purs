module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.ABC122.A as ABC122A
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  ABC122A.tests
