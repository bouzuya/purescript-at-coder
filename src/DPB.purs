-- dp_b
module DPB
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Effect (Effect)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe

foreign import modify :: forall h a. Fn3 Int (a -> a) (STArray h a) (ST h Unit)

foreign import peek :: forall h a. Fn2 Int (STArray h a) (ST h a)

foreign import poke :: forall h a. Fn3 Int a (STArray h a) (ST h a)

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  _ <- Stream.writeString Process.stdout Encoding.UTF8 (solve input) (pure unit)
  pure unit

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP = String.split (String.Pattern " ")

ints :: String -> Array Int
ints s =
  map
    (\x -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString x)))
    (splitBySP s)

type Args = { n :: Int, k :: Int, hs :: Array Int }

args :: String -> Args
args input =
  let
    lines = splitByNL input
    nks = splitBySP (Unsafe.unsafePartial (ArrayPartial.head lines))
    ns = Unsafe.unsafePartial (Array.unsafeIndex nks 0)
    n = Unsafe.unsafePartial (Maybe.fromJust (Int.fromString ns))
    ks = Unsafe.unsafePartial (Array.unsafeIndex nks 1)
    k = Unsafe.unsafePartial (Maybe.fromJust (Int.fromString ks))
    hs = ints (Unsafe.unsafePartial (ArrayPartial.last lines))
  in { n, k, hs }

solve :: String -> String
solve input = (show (solve' (args input))) <> "\n"

maxInt :: Int -> Int -> Int
maxInt = max

minInt :: Int -> Int -> Int
minInt = min

absInt :: Int -> Int
absInt = Ord.abs

solve' :: Args -> Int
solve' { n, k, hs } = ST.run do
  costs <- STArray.unsafeThaw (Array.replicate n top)
  _ <- runFn3 poke 0 0 costs
  ST.for 1 n \i -> do
    let h = Unsafe.unsafePartial (Array.unsafeIndex hs i)
    ST.for (maxInt 0 (i - k)) i \j -> do
      let h' = Unsafe.unsafePartial (Array.unsafeIndex hs j)
      cost' <- runFn2 peek j costs
      runFn3 modify i (minInt (cost' + (absInt (h' - h)))) costs
  runFn2 peek (n - 1) costs
