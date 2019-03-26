module AtCoder.ABC122.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe

counts :: String -> Map Int Int
counts s = MonadRec.tailRec go { a: Map.empty, i: 0, n: 0 }
  where
    ac = String.Pattern "AC"
    go { a, i, n } =
      case CodeUnits.indexOf' ac i s of
        Nothing -> MonadRec.Done a
        Just i' ->
          MonadRec.Loop
            { a: Map.insert (i' + 1) (n + 1) a
            , i: i' + 1
            , n: n + 1
            }

countRange :: Map Int Int -> Int -> Int -> Int
countRange a l r =
  let
    count1 = Maybe.fromMaybe 0 (map _.value (Map.lookupLE l a))
    count2 = Maybe.fromMaybe 0 (map _.value (Map.lookupLE r a))
  in
    count2 - count1

parseLine :: String -> Maybe (Tuple Int Int)
parseLine line =
  case String.split (String.Pattern " ") line of
    [ms, ns] -> do
      m <- Int.fromString ms
      n <- Int.fromString ns
      pure (Tuple m n)
    _ -> Nothing

solve :: String -> String
solve input =
  case solve'' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve'' :: String -> Either String String
solve'' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  (Tuple n q) <- Either.note "N Q" (parseLine =<< (Array.head lines))
  s <- Either.note "S" (Array.head =<< (Array.tail (Array.take 2 lines)))
  let
    c = counts s
    count = countRange c
    as =
      Array.mapMaybe
        (\qline -> do
          (Tuple l r) <- parseLine qline
          let
            il = l - 1
            ir = r - 1
          pure (show (count il ir)))
        (Array.drop 2 lines)
  if Array.length as /= q then Left "as length" else pure unit
  pure (Array.intercalate "\n" as)
