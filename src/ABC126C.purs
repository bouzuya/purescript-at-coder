-- abc126_c
module ABC126C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Ratio ((%))
import Data.Ratio as Ratio
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

solve' :: Int -> Int -> Number
solve' n k = ST.run do
  let
    n' = one % (BigInt.fromInt n)
    t' = one % (BigInt.fromInt 2)
  xRef <- STRef.new ((BigInt.fromInt 0) % one)
  ST.for 1 (n + 1) \i -> do
    tmpRef <- STRef.new n'
    nowRef <- STRef.new i
    ST.while
      do
        now <- STRef.read nowRef
        pure (now < k)
      do
        void (STRef.modify (mul 2) nowRef)
        void (STRef.modify (mul t') tmpRef)
    tmp <- STRef.read tmpRef
    STRef.modify (add tmp) xRef
  x <- STRef.read xRef
  pure
    ((BigInt.toNumber (Ratio.numerator x))
      / (BigInt.toNumber (Ratio.denominator x)))
