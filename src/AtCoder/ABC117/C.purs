module AtCoder.ABC117.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  { n, m } <-
    Either.note "line 1" do
      line <- Array.head lines
      case
        Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") line) of
        [n, m] -> pure { n, m }
        _ -> Maybe.Nothing
  xs <-
    Either.note "line 2" do
      line <- Array.last (Array.take 2 lines)
      pure
        (Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") line))
  pure (show (solve'' n m xs))

solve'' :: Int -> Int -> Array Int -> Int
solve'' n m xs
  | n >= m = 0
  | otherwise =
      let
        sorted = Array.sort xs
        ds =
          Array.reverse
            (Array.sort
              (map
                (\(Tuple a b) -> b - a)
                (Array.zip sorted (Array.drop 1 sorted))))
      in
        Foldable.sum (Array.drop (n - 1) ds)
