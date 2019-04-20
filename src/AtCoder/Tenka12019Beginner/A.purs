module AtCoder.Tenka12019Beginner.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  yes <-
    case
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern " ") (String.trim input)) of
      [a, b, c] -> Either.Right (between (min a b) (max a b) c)
      _ -> Either.Left "a b c"
  pure (if yes then "Yes" else "No")
