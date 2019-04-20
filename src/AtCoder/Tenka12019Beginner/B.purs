module AtCoder.Tenka12019Beginner.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  s <- Either.note "s" (Array.head (Array.drop 1 lines))
  k <- Either.note "k" ((Array.head (Array.drop 2 lines)) >>= Int.fromString)
  x <- Either.note "x" (CodeUnits.charAt (k - 1) s)
  pure
    (CodeUnits.fromCharArray
      (map
        (\c -> if c == x then c else '*')
        (CodeUnits.toCharArray s)))
