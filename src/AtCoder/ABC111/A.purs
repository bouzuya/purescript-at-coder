module AtCoder.ABC111.A
  ( solve
  ) where

import Prelude

import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  CodeUnits.fromCharArray
    (map
      (\c ->
        case c of
          '1' -> '9'
          '9' -> '1'
          _ -> Unsafe.unsafeCrashWith "invalid")
      (CodeUnits.toCharArray (String.trim input)))
