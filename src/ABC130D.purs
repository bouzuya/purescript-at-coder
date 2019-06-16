-- abc130_d
module ABC130D
  ( main
  , solve
  ) where

import Prelude

import Bouzuya.ST as BouzuyaST
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Array.ST as STArray
import Data.Array.ST.Partial as STArrayPartial
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe

main :: Effect Unit
main = do
  input <- FS.readTextFile Encoding.UTF8 "/dev/stdin"
  _ <- Stream.writeString Process.stdout Encoding.UTF8 (solve input) (pure unit)
  pure unit

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP = String.split (String.Pattern " ")

solve :: String -> String
solve input = ST.run do
  let
    lines = splitByNL input
    nk = splitBySP (Unsafe.unsafePartial (ArrayPartial.head lines))
    n =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (ArrayPartial.head nk))))
    k =
      Unsafe.unsafePartial (Maybe.fromJust (BigInt.fromString
        (Unsafe.unsafePartial (ArrayPartial.last nk))))
    as =
      map
        (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines)))
  sta <- STArray.unsafeThaw ((Array.replicate (n + 1) zero) :: Array BigInt)
  BouzuyaST.foreachWithIndex as \i a -> do
    pa <- Unsafe.unsafePartial (STArrayPartial.peek i sta)
    void
      (Unsafe.unsafePartial
        (STArrayPartial.poke (i + 1) (pa + (BigInt.fromInt a)) sta))
  sums <- STArray.unsafeFreeze sta
  iRef <- STRef.new 0
  jRef <- STRef.new 1
  countRef <- STRef.new (zero :: BigInt)
  ST.while
    do
      i <- STRef.read iRef
      j <- STRef.read jRef
      let iv = Unsafe.unsafePartial (Array.unsafeIndex sums i)
      let jv = Unsafe.unsafePartial (Array.unsafeIndex sums j)
      let v = jv - iv
      if v >= k
        then do
          void (STRef.modify (_ + BigInt.fromInt (n - j + 1)) countRef)
          void (STRef.write (i + 1) iRef)
          pure ((i + 1) <= n)
        else do
          if j + 1 > n
            then void (STRef.write (i + 1) iRef)
            else void (STRef.write (j + 1) jRef)
          pure ((i + 1) <= n)
    (pure unit)
  count <- STRef.read countRef
  pure ((BigInt.toString count) <> "\n")

