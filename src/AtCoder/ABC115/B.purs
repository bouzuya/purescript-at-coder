module AtCoder.ABC115.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable as Foldable
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
  let
    ints =
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern "\n") (String.trim input))
  n <- Either.note "n" (Array.head ints)
  let ps = Array.drop 1 ints
  if Array.length ps == n then pure unit else Either.Left "ps length"
  pure (show ((Foldable.sum ps) - ((Array.foldl max bottom ps) / 2)))
