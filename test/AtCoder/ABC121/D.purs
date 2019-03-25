-- https://atcoder.jp/contests/abc121/tasks/abc121_d
module Test.AtCoder.ABC121.D
  ( tests
  ) where

import Prelude

import AtCoder.ABC121.D as ABC121D
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Foldable as Foldable
import Data.String as String
import Effect.Class as Class
import Effect.Exception as Exception
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path as Path
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "ABC121.D" do
  let
    dir = "test/AtCoder/ABC121/D"
    solve = ABC121D.solve
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
      Assert.equal output (solve input)
