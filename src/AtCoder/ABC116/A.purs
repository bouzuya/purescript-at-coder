module AtCoder.ABC116.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
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
  let
    abbcca =
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern " ") (String.trim input))
  case abbcca of
    [ab, bc, ca] -> Right (show ((ab * bc) / 2))
    _ -> Left "invalid input"
