module AtCoder.ABC124.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  ab <-
    case
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern " ") (String.trim input)) of
      [a, b] -> Either.Right (Tuple.Tuple a b)
      _ -> Either.Left "a b"
  pure (show (solve'' ab))

solve'' :: Tuple Int Int -> Int
solve'' t =
  let
    Tuple.Tuple t' r = f t
    Tuple.Tuple _ r' = f t'
  in
    r + r'
  where
    f (Tuple.Tuple a b) =
      if a > b
        then Tuple.Tuple (Tuple.Tuple (a - 1) b) a
        else Tuple.Tuple (Tuple.Tuple a (b - 1)) b
