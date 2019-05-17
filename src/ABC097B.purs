-- abc097_b
module ABC097B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  n <- Either.note "n" (Int.fromString (String.trim input))
  pure ((show (solve' n)) <> "\n")

primes :: Int -> Array Int
primes n = ST.run do
  sta <- STArray.empty
  ST.for 2 (n + 1) \i -> do
    a <- STArray.unsafeFreeze sta
    unless (Array.elem i a) do
      void (STArray.push i sta)
  STArray.unsafeFreeze sta

solve' :: Int -> Int
solve' n = Maybe.fromMaybe 1 (Foldable.maximum expos)
  where
    expos :: Array Int
    expos = ST.run do
      sta <- STArray.empty
      ST.foreach (primes n) \p -> do
        iRef <- STRef.new 2
        ST.while
          do
            i <- STRef.read iRef
            let p' = Int.pow p i
            if p' <= n
              then do
                void (STArray.push p' sta)
                void (STRef.modify (add one) iRef)
                pure true
              else
                pure false
          (pure unit)
      STArray.unsafeFreeze sta
