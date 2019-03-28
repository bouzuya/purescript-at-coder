module Main
  ( main
  ) where

import Prelude

import AtCoder.ABC120.B as Solver
import Effect (Effect)
import Effect.Console as Console
import Node.Encoding as Encoding
import Node.FS.Sync as FS

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  output <- pure (Solver.solve input)
  Console.log output
