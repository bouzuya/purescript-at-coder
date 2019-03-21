module AtCoder.PracticeA
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
  { as, bcs, s } <-
    case String.split (String.Pattern "\n") (String.trim input) of
      [as, bcs, s] -> Right { as, bcs, s }
      _ -> Left "invalid lines"
  { bs, cs } <-
    case String.split (String.Pattern " ") bcs of
      [bs, cs] -> Right { bs, cs }
      _ -> Left "invalid 2nd line"
  a <- Either.note "a" (Int.fromString as)
  b <- Either.note "b" (Int.fromString bs)
  c <- Either.note "c" (Int.fromString cs)
  pure ((show (a + b + c)) <> " " <> s)
