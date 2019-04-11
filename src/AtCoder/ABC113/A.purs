module AtCoder.ABC113.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case
    Array.mapMaybe
      Int.fromString
      (String.split (String.Pattern " ") (String.trim input)) of
    [x, y] -> show (x + y / 2)
    _ -> Unsafe.unsafeCrashWith "line"
