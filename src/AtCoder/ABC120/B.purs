module AtCoder.ABC120.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  { a, b, k } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [as, bs, ks] -> do
        a <- Either.note "a" (Int.fromString as)
        b <- Either.note "b" (Int.fromString bs)
        k <- Either.note "k" (Int.fromString ks)
        pure { a, b, k }
      _ -> Left "invalid line"
  case solve'' a b k of
    Nothing -> Left "invalid input"
    Just n -> Right (show n)

solve'' :: Int -> Int -> Int -> Maybe Int
solve'' a b k =
  Array.index
    (Array.filter
      (\n -> ((a `mod` n) == 0) && ((b `mod` n) == 0))
      (Array.range (min a b) 1))
    (k - 1)

