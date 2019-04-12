module AtCoder.ABC113.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.Tuple as Tuple
import Debug.Trace as Debug
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" (Int.fromString =<< (Array.head lines))
  Tuple.Tuple t a <-
    Either.note "t a" do
      taLine <- Array.last (Array.take 2 lines)
      case
        Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") taLine) of
        [t, a] -> Maybe.Just (Tuple.Tuple t a)
        _ -> Maybe.Nothing
  hs <-
    Either.note "hs" do
      hsLine <- Array.last lines
      pure
        (Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") hsLine))
  pure (show (solve'' n t a hs))

solve'' :: Int -> Int -> Int -> Array Int -> Int
solve'' n t a hs =
  let
    tn = Int.toNumber t
    an = Int.toNumber a
  in
    Maybe.maybe'
      (\_ -> Unsafe.unsafeCrashWith "hs is empty")
      Tuple.fst
      (Foldable.minimumBy
        (comparing Tuple.snd)
        (Array.mapWithIndex
          (\i h -> Tuple.Tuple (i + 1) (Ord.abs (an - (tn - h * 0.006))))
          (map Int.toNumber hs)))
