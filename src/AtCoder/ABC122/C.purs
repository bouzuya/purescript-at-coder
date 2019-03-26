module AtCoder.ABC122.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe

indices :: String -> Array Int
indices s = MonadRec.tailRec go { i: 0, a: [] }
  where
    ac = String.Pattern "AC"
    go { i, a } =
      case CodeUnits.indexOf' ac i s of
        Nothing -> MonadRec.Done a
        Just i' -> MonadRec.Loop { i: i' + 1, a: Array.snoc a i' }

countIndices :: Array Int -> Int -> Int -> Int
countIndices a l r = Maybe.fromMaybe 0 do
  i1 <- Array.findIndex (l <= _) a
  i2 <- Array.findLastIndex (_ <= r) a
  pure (i2 - i1 + 1)

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
  s <- Either.note "S" (Array.head (Array.drop 1 lines))
  let
    count = countIndices (indices s)
    as =
      Array.mapMaybe
        (\qline -> do
          (Tuple l r) <- parseLine qline
          let
            il = l - 1
            ir = r - 2
          pure (show (count il ir)))
        (Array.drop 2 lines)
  if Array.length as /= q then Left "as length" else pure unit
  pure (Array.intercalate "\n" as)
