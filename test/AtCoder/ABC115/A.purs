-- https://atcoder.jp/contests/abc115/tasks/abc115_a
module Test.AtCoder.ABC115.A
  ( tests
  ) where

import Prelude

import AtCoder.ABC115.A as Solver
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
tests = TestUnit.suite "ABC115.A" do
  let dir = "test/AtCoder/ABC115/A"
  TestUnit.test "examples" do
    files <-
      Class.liftEffect
        (map (map (Path.concat <<< (Array.snoc [dir]))) (FS.readdir dir))
    Foldable.for_ files \file -> do
      { input, output } <- Class.liftEffect do
        content <- FS.readTextFile Encoding.UTF8 file
        case String.split (String.Pattern "---\n") content of
          [i, o] -> pure { input: i, output: o }
          a -> Exception.throw ("invalid example: " <> file <> ":" <> show a)
      Assert.equal output ((Solver.solve input) <> "\n")

  TestUnit.test "my examples" do
    let
      examples =
        []
    Foldable.for_ examples \(Tuple input output) -> do
      Assert.equal output (Solver.solve (input <> "\n"))
