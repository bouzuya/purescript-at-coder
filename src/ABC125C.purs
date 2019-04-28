-- abc125_c
module ABC125C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
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

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  asLine <- Either.note "vs" (Array.last lines)
  let as = Array.mapMaybe Int.fromString (splitBySP asLine)
  pure ((show (solve' n as)) <> "\n")

solve' :: Int -> Array Int -> Int
solve' n as = ST.run do
  sta1 <- STArray.unsafeThaw (Array.replicate n 1)
  sta2 <- STArray.unsafeThaw (Array.replicate n 1)
  xRef <- STRef.new 0
  ST.for 0 n \i -> do
    x <- STRef.read xRef
    let
      y = Unsafe.unsafePartial (Maybe.fromJust (Array.index as i))
      x' = gcd x y
    void (STRef.write x' xRef)
    void (STArray.poke i x' sta1)
  void (STRef.write 0 xRef)
  ST.for 0 n \i -> do
    x <- STRef.read xRef
    let
      y = Unsafe.unsafePartial (Maybe.fromJust (Array.index as (n - i - 1)))
      x' = gcd x y
    void (STRef.write x' xRef)
    void (STArray.poke (n - i - 1) x' sta2)
  a1 <- (STArray.unsafeFreeze sta1)
  a2 <- (STArray.unsafeFreeze sta2)
  maxRef <- STRef.new bottom
  ST.for 0 n \i -> do -- i を抜いたとき
    x' <- map (Maybe.fromMaybe 0) (STArray.peek (i - 1) sta1)
    y' <- map (Maybe.fromMaybe 0) (STArray.peek (i + 1) sta2)
    void (STRef.modify (max (gcd x' y')) maxRef)
  STRef.read maxRef
