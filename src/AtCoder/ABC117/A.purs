module AtCoder.ABC117.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Number as Number
import Data.String as String
import Partial.Unsafe as Unsafe

parseLine :: String -> Array Number
parseLine s =
  Array.mapMaybe Number.fromString (String.split (String.Pattern " ") s)

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  { t, x } <-
    case parseLine (String.trim input) of
      [t, x] -> Right { t, x }
      _ -> Left "invalid line"
  pure (show (t / x))
