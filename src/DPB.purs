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
import Data.Array.ST.Partial as STArrayPartial
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

foreign import modifyImpl ::
  forall h a. Int -> (a -> a) -> STArray h a -> ST h Unit

modify :: forall h a. Partial => Int -> (a -> a) -> STArray h a -> ST h Unit
modify = modifyImpl

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
ints s = Array.mapMaybe Int.fromString (splitBySP s)

solve :: String -> String
solve input =
  let
    lines = splitByNL input
    nks = splitBySP (Unsafe.unsafePartial (ArrayPartial.head lines))
    ns = Unsafe.unsafePartial (Array.unsafeIndex nks 0)
    n = Unsafe.unsafePartial (Maybe.fromJust (Int.fromString ns))
    ks = Unsafe.unsafePartial (Array.unsafeIndex nks 1)
    k = Unsafe.unsafePartial (Maybe.fromJust (Int.fromString ks))
    hs = ints (Unsafe.unsafePartial (ArrayPartial.last lines))
  in (show (solve' n k hs)) <> "\n"

solve' :: Int -> Int -> Array Int -> Int
solve' n k hs = ST.run do
  costs <- STArray.unsafeThaw (Array.replicate n top)
  _ <- Unsafe.unsafePartial (STArrayPartial.poke 0 0 costs)
  ST.for 1 n \i -> do
    let h = Unsafe.unsafePartial (Array.unsafeIndex hs i)
    ST.for (max 0 (i - k)) i \j -> do
      let h' = Unsafe.unsafePartial (Array.unsafeIndex hs j)
      cost' <- Unsafe.unsafePartial (STArrayPartial.peek j costs)
      _ <-
        Unsafe.unsafePartial (modify i (min (cost' + (Ord.abs (h' - h)))) costs)
      pure unit
  Unsafe.unsafePartial (STArrayPartial.peek (n - 1) costs)
