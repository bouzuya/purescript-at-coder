module AtCoder.ABC123.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let
    xs =
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern "\n") (String.trim input))
    sorted =
      Array.sortWith
        Tuple.snd
        (map (\x -> Tuple.Tuple x (x `mod` 10)) xs)
  pure
    (show
      (Array.foldl
        (\acc (Tuple.Tuple index (Tuple.Tuple x y)) ->
          if index == (Array.length xs - 1)
            then acc + x
            else
              case y of
                0 -> acc + x
                _ -> acc + ((x / 10) * 10) + 10)
        0
        (Array.mapWithIndex
          Tuple.Tuple
          ((Array.filter (\(Tuple.Tuple _ y) -> y == 0) sorted)
          <>
          (Array.reverse (Array.filter (\(Tuple.Tuple _ y) -> y /= 0) sorted))))))


