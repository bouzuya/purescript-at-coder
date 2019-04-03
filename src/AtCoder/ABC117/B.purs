module AtCoder.ABC117.B
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
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  line2 <- Either.note "line 2" (Array.head (Array.drop 1 lines))
  let
    sorted =
      Array.reverse
        (Array.sort
          (Array.mapMaybe
            Int.fromString
            (String.split (String.Pattern " ") line2)))
  max <- Either.note "max" (Array.head sorted)
  pure (if max < Foldable.sum (Array.drop 1 sorted) then "Yes" else "No")
