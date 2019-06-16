-- abc130_a
module ABC130A
  ( main
  , solve
  ) where

import Prelude

import Data.Array.Partial as ArrayPartial
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
    xa =
      map
        (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
        (splitBySP (String.trim input))
    x = Unsafe.unsafePartial (ArrayPartial.head xa)
    a = Unsafe.unsafePartial (ArrayPartial.last xa)
  in
    (show (if x < a then 0 else 10)) <> "\n"
