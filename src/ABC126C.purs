-- abc126_c
module ABC126C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Ref as Ref
import Math as Math
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  Tuple.Tuple n k <- Either.note "n k" (int2 (String.trim input))
  pure ((show (solve' n k)) <> "\n")

f :: Int -> Number -> Int
f a k = ST.run do
  iRef <- STRef.new 0
  ST.while
    do
      i <- STRef.read iRef
      pure ((Int.toNumber (Bits.shl 1 i)) < k)
    (void (STRef.modify (add one) iRef))
  i <- STRef.read iRef
  pure (if i > a then 0 else i)

solve' :: Int -> Int -> Number
solve' n k = ST.run do
  let
    n' = Int.toNumber n
    k' = Int.toNumber k
    a =
      Unsafe.unsafePartial
        (Maybe.fromJust (Int.fromNumber (Math.ceil ((Math.log k') / Math.ln2))))
  xRef <- STRef.new 0.0
  ST.for 1 (n + 1) \i -> do
    let j = f a (k' / (Int.toNumber i))
    if j == 0
      then pure unit
      else
        void
          (STRef.modify (add (1.0 / ((Int.toNumber (Bits.shl 1 j)) * n'))) xRef)
  STRef.read xRef

