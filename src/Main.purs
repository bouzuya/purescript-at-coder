module Main
  ( main
  ) where

import Prelude

import AtCoder.ABC085.C as ABC085C
import Effect (Effect)
import Effect.Console as Console
import Node.Encoding as Encoding
import Node.FS.Sync as FS

solve :: String -> String
solve = ABC085C.solve

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  output <- pure (solve input)
  Console.log output
