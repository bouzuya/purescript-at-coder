module AtCoder.ABC049.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String

tfs :: Array (String -> Maybe String)
tfs =
  map
    (String.stripPrefix <<< String.Pattern)
    [ "dream"
    , "dreamer"
    , "erase"
    , "eraser"
    ]

solve :: String -> String
solve input = solve' (String.trim input)

solve' :: String -> String
solve' s =
  case Array.catMaybes (map (\f -> f s) tfs) of
    [] -> "NO"
    ss' ->
      case Array.find String.null ss' of
        Just _ -> "YES"
        Nothing ->
          Maybe.maybe
            "NO"
            (const "YES")
            (Array.find (\s' -> solve' s' == "YES") ss')
