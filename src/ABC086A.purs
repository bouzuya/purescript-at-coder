-- abc086_a
module ABC086A
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

splitBySP :: String -> Array String
splitBySP = String.split (String.Pattern " ")

solve :: String -> String
solve input =
  let
    ab = splitBySP (String.trim input)
    a =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex ab 0))))
    b =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (Array.unsafeIndex ab 1))))
  in
    (if (Int.even (a * b)) then "Even" else "Odd") <> "\n"
