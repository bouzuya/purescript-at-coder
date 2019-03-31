module AtCoder.ABC119.A
  ( solve
  ) where

import Prelude

import Data.String as String

solve :: String -> String
solve input
  | (String.trim input) <= "2019/04/30" = "Heisei"
  | otherwise = "TBD"
