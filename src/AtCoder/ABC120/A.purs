module AtCoder.ABC120.A
  ( solve
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
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
  { a, b, c } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [as, bs, cs] -> do
        a <- Either.note "a" (Int.fromString as)
        b <- Either.note "b" (Int.fromString bs)
        c <- Either.note "c" (Int.fromString cs)
        pure { a, b, c }
      _ -> Left "invalid line"
  pure (show (min (b / a) c))
