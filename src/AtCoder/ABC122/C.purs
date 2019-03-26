module AtCoder.ABC122.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class (Step)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe

counts :: String -> Array Int
counts s = STArray.run do
  a <- STArray.unsafeThaw (Array.replicate (String.length s) 0)
  MonadRec.tailRecM go { a, i: 0, n: 0 }
  where
    ac :: Pattern
    ac = String.Pattern "AC"

    go :: forall h.
      { a :: STArray h Int, i :: Int, n :: Int }
      -> ST h (Step { a :: STArray h Int, i :: Int, n :: Int } (STArray h Int))
    go { a, i, n } =
      let
        fill j j' = do
          _ <- STArray.modify j (const n) a
          if j == j' then pure a else fill (j + 1) j'
      in
        case CodeUnits.indexOf' ac i s of
          Nothing -> do
            _ <- fill i ((String.length s) - 1)
            pure (MonadRec.Done a)
          Just i' -> do
            _ <- fill i i'
            pure (MonadRec.Loop { a, i: i' + 1, n: n + 1 })

countRange :: Array Int -> Int -> Int -> Int
countRange a l r =
  let
    i1 = Unsafe.unsafePartial (Array.unsafeIndex a l)
    i2 = Unsafe.unsafePartial (Array.unsafeIndex a r)
  in
    i2 - i1

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
