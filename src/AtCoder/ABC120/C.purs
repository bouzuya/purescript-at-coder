module AtCoder.ABC120.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits

solve :: String -> String
solve input = show (solve' input)

solve' :: String -> Int
solve' input = MonadRec.tailRec go { c0: 0, c1: 0, i: 0 }
  where
    s = String.trim input

    go { c0, c1, i } =
      case CodeUnits.charAt i s of
        Nothing -> MonadRec.Done ((min c0 c1) * 2)
        Just c -> MonadRec.Loop
          { c0: if c == '0' then c0 + 1 else c0
          , c1: if c == '1' then c1 + 1 else c1
          , i: i + 1
          }
