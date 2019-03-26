module AtCoder.ABC122.A
  ( solve
  ) where

import Prelude

import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case String.trim input of
    "A" -> "T"
    "C" -> "G"
    "G" -> "C"
    "T" -> "A"
    _ -> Unsafe.unsafeCrashWith ("invalid input: " <> input)
