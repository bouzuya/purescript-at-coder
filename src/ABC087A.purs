-- abc087_a
module ABC087A
  ( main
  , solve
  ) where

import Prelude

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
solve input =
  let
    lines = splitByNL input
    x =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 0))))
    a =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 1))))
    b =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex lines 2))))
  in
    (show ((x - a) `mod` b)) <> "\n"
