-- abc127_d -- FIXME: TLE
module ABC127D
  ( main
  , solve
  ) where

import Prelude

import Bouzuya.ST.PriorityQueue as PriorityQueue
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect (Effect)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  output <- pure (solve input)
  writeStdout output

writeStdout :: String -> Effect Unit
writeStdout s =
  void (Stream.writeString Process.stdout Encoding.UTF8 s (pure unit))

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  nmLine <- Either.note "nmLine" (Array.head (Array.take 1 lines))
  asLine <-
    Either.note "asLine" (Array.last (Array.take 2 lines))
  let bcLines = Array.drop 2 lines
  Tuple.Tuple n m <- Either.note "n m " (int2 nmLine)
  let
    as =
      Array.mapMaybe
        ((map (flip Tuple.Tuple 1)) <<< Int.fromString)
        (splitBySP asLine)
    bcs =
      Array.mapMaybe ((map Tuple.swap) <<< int2) bcLines
  pure ((BigInt.toString (solve' n m as bcs)) <> "\n")

solve' :: Int -> Int -> Array (Tuple Int Int) -> Array (Tuple Int Int) -> BigInt
solve' n _ as bcs = ST.run do
  sta <- STArray.unsafeThaw as
  _ <- STArray.pushAll bcs sta
  q <- PriorityQueue.fromSTArray sta
  countRef <- STRef.new 0
  resultRef <- STRef.new zero
  ST.while
    do
      count <- STRef.read countRef
      maxMaybe <- PriorityQueue.dequeue q
      let
        (Tuple.Tuple v c) = Unsafe.unsafePartial (Maybe.fromJust maxMaybe)
        count' = count + c
        continue = count' < n
        v' = BigInt.fromInt v -- max 10^9
        c' = BigInt.fromInt (min (n - count) c) -- max 10^5
      _ <- STRef.write count' countRef
      _ <- STRef.modify (add (c' * v')) resultRef
      pure continue
    (pure unit)
  STRef.read resultRef
