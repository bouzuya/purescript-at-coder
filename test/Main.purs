module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.AtCoder.ABC081.B as ABC081B
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  ABC081B.tests
