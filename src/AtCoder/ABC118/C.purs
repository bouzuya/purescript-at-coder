module AtCoder.ABC118.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
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
  let lines = String.split (String.Pattern "\n") (String.trim input)
  ns <- Either.note "ns" (Array.head lines)
  n <- Either.note "n" (Int.fromString ns)
  ass <- Either.note "ass" (Array.head (Array.drop 1 lines))
  let
    as = Array.mapMaybe Int.fromString (String.split (String.Pattern " ") ass)
    a = Unsafe.unsafePartial (Array.unsafeIndex as 0)
  pure (show (Array.foldl gcd a as))
