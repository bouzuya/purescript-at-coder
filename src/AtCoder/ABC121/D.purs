module AtCoder.ABC121.D
  ( solve
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  let
    line = String.split (String.Pattern " ") (String.trim input)
    { a, b } =
      case map BigInt.fromString line of
        [Just a, Just b] -> { a, b }
        _ -> Unsafe.unsafeCrashWith "BigInt.fromString"
  in BigInt.toString (solve'' a b)

solve'' :: BigInt -> BigInt -> BigInt
solve'' a b
  | a == b = a
  | BigInt.even a =
    let
      n = b - a + one
      m = if BigInt.even n then n else (n - one)
      m' = if (BigInt.rem m (BigInt.fromInt 4)) == zero then zero else one
    in
      BigInt.xor (if BigInt.even n then zero else b) m'
  | otherwise =
      BigInt.xor a (solve'' (a + one) b)
