module AtCoder.ABC118.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  ns <- Either.note "ns" (Array.head lines)
  n <- Either.note "n" (Int.fromString ns)
  ass <- Either.note "ass" (Array.head (Array.drop 1 lines))
  let as = Array.mapMaybe Int.fromString (String.split (String.Pattern " ") ass)
  pure (show (g as))

g :: Array Int -> Int
g xs
  | Array.length xs == 1 =
    Unsafe.unsafePartial (Maybe.fromJust (Array.head xs))
  | otherwise = g (f xs)

f :: Array Int -> Array Int
f xs =
  let
    xs' = Array.reverse (Array.sort xs)
    x = Unsafe.unsafePartial (Array.unsafeIndex xs' 0)
    os = Array.drop 1 xs'
    m =
      Array.foldl
        (\acc o ->
          case acc, (x `mod` o) of
            Nothing, 0 -> Nothing
            Nothing, m' -> Just m'
            Just acc', 0 -> Just acc'
            Just acc', m' -> Just (min acc' m'))
        Nothing
        os
  in
    case m of
      Nothing -> os
      Just m' -> Array.snoc os m'
