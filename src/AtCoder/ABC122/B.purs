module AtCoder.ABC122.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String

solve :: String -> String
solve input =
  show
    (Maybe.fromMaybe
      0
      (map
        _.max
        (Array.foldRecM
          go
          { cur: 0, max: 0 }
          (String.split (String.Pattern "") (String.trim input)))))
  where
    isACGT =
      case _ of
        "A" -> true
        "C" -> true
        "G" -> true
        "T" -> true
        _ -> false

    go { cur, max: max' } c
      | isACGT c = Just { max: max max' (cur + 1) , cur: cur + 1 }
      | otherwise = Just { max: max', cur: 0 }
