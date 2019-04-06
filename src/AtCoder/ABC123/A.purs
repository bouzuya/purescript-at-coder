module AtCoder.ABC123.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let
    lines =
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern "\n") (String.trim input))
  { init: abcde, last: k } <- Either.note "unsnoc" (Array.unsnoc lines)
  pure
    if
      Maybe.isJust
        (Array.find
          identity
          (do
            x <- abcde
            x' <- abcde
            pure (x - x' > k)))
      then ":("
      else "Yay!"
