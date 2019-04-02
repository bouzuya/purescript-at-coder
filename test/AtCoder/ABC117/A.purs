-- https://atcoder.jp/contests/abc117/tasks/abc117_a
module Test.AtCoder.ABC117.A
  ( tests
  ) where

import Prelude

import AtCoder.ABC117.A as Solver
import Data.Array as Array
import Data.Foldable as Foldable
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Class as Class
import Effect.Exception as Exception
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path as Path
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC117.A" do
  TestUnit.test "my examples" do
    let
      examples =
        [ Tuple "8 3" "2.6666666666666665"
        , Tuple "99 1" "99.0"
        , Tuple "1 100" "0.01"
        ]
    Foldable.for_ examples \(Tuple input output) -> do
      Assert.equal output (Solver.solve (input <> "\n"))
