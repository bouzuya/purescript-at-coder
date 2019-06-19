-- abc087_b
module ABC087B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
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
    a =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 0))))
    b =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 1))))
    c =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 2))))
    x =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 3))))
  countRef <- STRef.new 0
  ST.for 0 (a + 1) \i -> do
    ST.for 0 (b + 1) \j -> do
      ST.for 0 (c + 1) \k -> do
        if (500 * i + 100 * j + 50 * k) == x
          then void (STRef.modify (add one) countRef)
          else pure unit
  count <- STRef.read countRef
  pure ((show count) <> "\n")
