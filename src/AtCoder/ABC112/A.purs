module AtCoder.ABC112.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.String as String
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
  case n of
    1 -> pure "Hello World"
    2 -> do
      pure
        (show
          (Array.foldl
            (+)
            0
            (Array.mapMaybe Int.fromString (Array.drop 1 lines))))
    _ -> Either.Left "invalid n"
