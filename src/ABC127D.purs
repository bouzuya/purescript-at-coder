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
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe

main :: Effect Unit
main = Aff.launchAff_ do
  input <- readStdin
  output <- pure (solve input)
  Class.liftEffect (writeStdout output)

readStdin :: Aff.Aff String
readStdin =
  let r = Process.stdin
  in
    Aff.makeAff
      (\callback -> do
        ref <- Ref.new ""
        Stream.onDataString r Encoding.UTF8 \s -> do
          buffer <- Ref.read ref
          Ref.write (buffer <> s) ref
        Stream.onEnd r do
          buffer <- Ref.read ref
          callback (pure buffer)
        pure mempty)

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
    Either.note "asLine" (Array.head (Array.drop 1 (Array.take 2 lines)))
  let bcLines = Array.drop 2 lines
  Tuple.Tuple n m <- Either.note "n m " (int2 nmLine)
  let as = Array.mapMaybe Int.fromString (splitBySP asLine)
  let bcs = Array.mapMaybe int2 bcLines
  pure ((BigInt.toString (solve' n m as bcs)) <> "\n")

type Count = Int
type Value = Int
data X = X Count Value
derive instance eqX :: Eq X
instance ordX :: Ord X where
  compare (X _ a) (X _ b) = compare a b

solve' :: Int -> Int -> Array Int -> Array (Tuple Int Int) -> BigInt
solve' n _ as bcs = ST.run do
  q <- PriorityQueue.fromArray (map (X 1) as)
  ST.foreach bcs \(Tuple.Tuple b c) -> do
    PriorityQueue.enqueue (X b c) q
  countRef <- STRef.new 0
  resultRef <- STRef.new zero
  ST.while
    do
      count <- STRef.read countRef
      maxMaybe <- PriorityQueue.dequeue q
      let
        (X c v) = Unsafe.unsafePartial (Maybe.fromJust maxMaybe)
        count' = count + c
        continue = count' < n
        v' = BigInt.fromInt v
        c' = BigInt.fromInt (min (n - count) c)
      void (STRef.write count' countRef)
      void (STRef.modify (add (c' * v')) resultRef)
      pure continue
    (pure unit)
  STRef.read resultRef
