module AtCoder.ABC124.B
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
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" (Array.head lines >>= Int.fromString)
  hs <-
    Either.note "hs" do
      hsLine <- Array.last lines
      pure
        (Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") hsLine))
  pure (show (solve'' n hs))

solve'' :: Int -> Array Int -> Int
solve'' n hs =
  let zipped = Array.zip (Array.drop 1 hs) hs
  in
    1
    + (Array.length
        (Array.filter
          (\(Tuple.Tuple index (Tuple.Tuple h p)) ->
            p <= h && (Array.all (\x -> x <= h) (Array.take (index + 1) hs)))
          (Array.mapWithIndex Tuple.Tuple zipped)))
