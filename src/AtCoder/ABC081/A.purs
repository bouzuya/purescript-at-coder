module AtCoder.ABC081.A
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.String as String

solve :: String -> String
solve input =
  show
    (Array.length
      (Array.filter
        (eq "1")
        (String.split
          (String.Pattern "")
          (String.trim input))))
