-- https://atcoder.jp/contests/abc121/tasks/abc121_d
module Test.AtCoder.ABC121.D
  ( tests
  ) where

import Prelude

import AtCoder.ABC121.D as ABC121D
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
      Assert.equal output ((solve input) <> "\n")

  TestUnit.test "my examples" do
    -- 2 x 3                         (n = 2) = 1
    -- 2 x 3 x 4                     (n = 3) = 1 xor 4 = 5
    -- 2 x 3 x 4 x 5                 (n = 4) = 0
    -- 2 x 3 x 4 x 5 x 6             (n = 5) = 0 xor 6 = 6
    -- 2 x 3 x 4 x 5 x 6 x 7         (n = 6) = 1
    -- 2 x 3 x 4 x 5 x 6 x 7 x 8     (n = 7) = 1 xor 8 = 9
    -- 2 x 3 x 4 x 5 x 6 x 7 x 8 x 9 (n = 8) = 0
    let
      examples =
        [ Tuple "2 3" "1"
        , Tuple "2 4" "5"
        , Tuple "2 5" "0"
        , Tuple "2 6" "6"
        , Tuple "2 7" "1"
        , Tuple "2 8" "9"
        , Tuple "2 9" "0"
        ]
    Foldable.for_ examples \(Tuple input output) -> do
      Assert.equal output (ABC121D.solve (input <> "\n"))
