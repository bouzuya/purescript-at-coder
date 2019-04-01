module AtCoder.ABC118.A
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
  { a, b } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [as, bs] -> do
        a <- Either.note "a" (Int.fromString as)
        b <- Either.note "b" (Int.fromString bs)
        pure { a, b }
      _ -> Left "invalid line"
  pure (show (solve'' a b))

solve'' :: Int -> Int -> Int
solve'' a b
  | (b `mod` a) == 0 = a + b
  | otherwise = b - a
