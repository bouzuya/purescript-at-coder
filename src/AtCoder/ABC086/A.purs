module AtCoder.ABC086.A
  ( solve
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  { as, bs } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [as, bs] -> Right { as, bs }
      _ -> Left "invalid line"
  a <- Either.note "a" (Int.fromString as)
  b <- Either.note "b" (Int.fromString bs)
  pure (if Int.odd (a * b) then "Odd" else "Even")
