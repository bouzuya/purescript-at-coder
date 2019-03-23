module AtCoder.ABC049.C
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String

ts :: Array String
ts =
  [ "dream"
  , "dreamer"
  , "erase"
  , "eraser"
  ]

solve :: String -> String
solve input
  | solve' (Set.fromFoldable [String.trim input]) = "YES"
  | otherwise = "NO"

solve' :: Set String -> Boolean
solve' ss =
  let
    keys = Set.toUnfoldable ss
  in
    case Array.head keys of
      Nothing -> false
      Just s ->
        let
          nexts = solve'' s
        in
          case Array.find String.null nexts of
            Just _ -> true
            Nothing ->
              solve' (Set.fromFoldable ((Array.drop 1 keys) <> nexts))

solve'' :: String -> Array String
solve'' s =
  Array.catMaybes (map (\t -> String.stripPrefix (String.Pattern t) s) ts)
