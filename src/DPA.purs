-- dp_a
module DPA
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  n <-
    Either.note "n"
      (Int.fromString (Unsafe.unsafePartial (ArrayPartial.head lines)))
  let
    hs =
      Array.mapMaybe
        Int.fromString
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines)))
  pure ((show (solve' n hs)) <> "\n")

solve' :: Int -> Array Int -> Int
solve' n hs = ST.run do
  let
    unsafePeek i xs =
      map
        (\x -> Unsafe.unsafePartial (Maybe.fromJust x))
        (STArray.peek i xs)
  costs <- STArray.unsafeThaw (Array.replicate n 0)
  let
    h_0 = Unsafe.unsafePartial (Array.unsafeIndex hs 0)
    h_1 = Unsafe.unsafePartial (Array.unsafeIndex hs 1)
  _ <- STArray.poke 0 0 costs
  _ <- STArray.poke 1 (Ord.abs (h_1 - h_0)) costs
  ST.for 2 n \i -> do
    cost2 <- unsafePeek (i - 2) costs
    cost1 <- unsafePeek (i - 1) costs
    let
      h2 = Unsafe.unsafePartial (Array.unsafeIndex hs (i - 2))
      h1 = Unsafe.unsafePartial (Array.unsafeIndex hs (i - 1))
      h = Unsafe.unsafePartial (Array.unsafeIndex hs i)
      min' = min (cost2 + (Ord.abs (h2 - h))) (cost1 + (Ord.abs (h1 - h)))
    _ <- STArray.poke i min' costs
    pure unit
  unsafePeek (n - 1) costs
