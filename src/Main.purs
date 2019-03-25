module Main
  ( main
  ) where

import Prelude

import AtCoder.ABC121.D as ABC121D
import Effect (Effect)
import Node.Encoding as Encoding
import Node.FS.Sync as FS

solve :: String -> String
solve = ABC121D.solve

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  output <- pure (solve input)
  FS.appendTextFile Encoding.UTF8 "/dev/stdout" output
