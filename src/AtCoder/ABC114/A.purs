module AtCoder.ABC114.A
  ( solve
  ) where

import Prelude

import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  x <- Either.note "x" (Int.fromString (String.trim input))
  pure
    if x == 3 || x == 5 || x == 7
      then "YES"
      else "NO"
