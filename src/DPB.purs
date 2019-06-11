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
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class as Class
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
  output <- pure (solve input)
  Class.liftEffect (writeStdout output)

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

ints :: String -> Array Int
ints s = Array.mapMaybe Int.fromString (splitBySP s)

unsafeIndex :: forall a. Int -> Array a -> a
unsafeIndex i xs = Unsafe.unsafePartial (Array.unsafeIndex xs i)

unsafeIndex' :: forall a h. Int -> STArray h a -> ST h a
unsafeIndex' i xs = Unsafe.unsafePartial (STArrayPartial.peek i xs)

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  Tuple.Tuple n k <-
    Either.note "n k"
      (int2 (Unsafe.unsafePartial (ArrayPartial.head lines)))
  let hs = ints (Unsafe.unsafePartial (ArrayPartial.last lines))
  pure ((show (solve' n k hs)) <> "\n")

solve' :: Int -> Int -> Array Int -> Int
solve' n k hs = ST.run do
  costs <- STArray.unsafeThaw (Array.replicate n top)
  _ <- Unsafe.unsafePartial (STArrayPartial.poke 0 0 costs)
  ST.for 1 n \i -> do
    let h = unsafeIndex i hs
    ST.for (max 0 (i - k)) i \j -> do
      let h' = unsafeIndex j hs
      cost' <- unsafeIndex' j costs
      _ <-
        Unsafe.unsafePartial (modify i (min (cost' + (Ord.abs (h' - h)))) costs)
      pure unit
  unsafeIndex' (n - 1) costs
