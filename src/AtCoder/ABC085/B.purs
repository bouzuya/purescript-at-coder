module AtCoder.ABC085.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.Traversable as Traversable
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  as <-
    Traversable.traverse
      ((Either.note "as") <<< Int.fromString)
      (Array.drop 1 (String.split (String.Pattern "\n") (String.trim input)))
  pure (show (solve'' as))

solve'' :: Array Int -> Int
solve'' xs = Array.length (Array.group (Array.sort xs))
