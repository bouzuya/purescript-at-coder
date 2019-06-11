-- dp_b
module DPB
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Ord as Ord
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

ints :: String -> Array Int
ints s = Array.mapMaybe Int.fromString (splitBySP s)

unsafeIndex :: forall a. Int -> Array a -> a
unsafeIndex i xs = Unsafe.unsafePartial (Array.unsafeIndex xs i)

unsafeIndex' :: forall a h. Int -> STArray h a -> ST h a
unsafeIndex' i xs =
  map (\x -> Unsafe.unsafePartial (Maybe.fromJust x)) (STArray.peek i xs)

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
  _ <- STArray.poke 0 0 costs
  ST.for 1 n \i -> do
    let h = unsafeIndex i hs
    ST.for (max 0 (i - k)) i \j -> do
      cost <- unsafeIndex' j costs
      let h' = unsafeIndex j hs
      void (STArray.modify i (min (cost + (Ord.abs (h' - h)))) costs)
  unsafeIndex' (n - 1) costs
