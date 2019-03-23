module AtCoder.ABC049.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Maybe (Maybe(..))
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
  | solve' [String.trim input] = "YES"
  | otherwise = "NO"

solve' :: Array String -> Boolean
solve' = MonadRec.tailRec go
  where
    go ss =
      case Array.head ss of
        Nothing -> MonadRec.Done false
        Just s ->
          let
            nexts = solve'' s
          in
            case Array.find String.null nexts of
              Just _ -> MonadRec.Done true
              Nothing -> MonadRec.Loop (Array.nub ((Array.drop 1 ss) <> nexts))

solve'' :: String -> Array String
solve'' s =
  Array.catMaybes (map (\t -> String.stripPrefix (String.Pattern t) s) ts)
