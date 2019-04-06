module AtCoder.ABC116.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  hs <- Either.note "hs" do
    hsLine <- Array.last lines
    pure
      (Array.mapMaybe Int.fromString (String.split (String.Pattern " ") hsLine))
  pure (show (solve'' n hs))

solve'' :: Int -> Array Int -> Int
solve'' n hs = MonadRec.tailRec go { count: 0, i: 0, updated: true }
  where
    go { count, i, updated }
      | not updated = MonadRec.Done count
      | otherwise =
          let
            newCount =
              Array.length
                (Array.filter
                  NonEmptyArray.head
                  (Array.group (map (\h -> h > i) hs)))
          in
            MonadRec.Loop
              { count: count + newCount, i: i + 1, updated: newCount > 0 }
