module AtCoder.ABC123.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt as BigInt
import Data.BigInt as BitInt
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let
    lines =
      Array.mapMaybe
        BigInt.fromString
        (String.split (String.Pattern "\n") (String.trim input))
    n = Unsafe.unsafePartial (Array.unsafeIndex lines 0)
    xs = Array.drop 1 lines
    minX =
      Array.foldl
        (\a b ->
          case a of
            Maybe.Nothing -> Maybe.Just b
            Maybe.Just a' -> Maybe.Just (min a' b))
        Maybe.Nothing
        xs
  minX' <- Either.note "minX" minX
  pure
    (BigInt.toString
      ( (n / minX')
      + (if (BitInt.rem n minX') /= zero
          then BigInt.fromInt 1
          else BigInt.fromInt 0)
      + (BigInt.fromInt 4)))
