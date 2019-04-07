module AtCoder.ABC115.A
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
  d <- Either.note "d" (Int.fromString (String.trim input))
  pure ("Christmas" <> (String.joinWith "" (Array.replicate (25 - d) " Eve")))
