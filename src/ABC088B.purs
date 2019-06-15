-- abc088_b
module ABC088B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple as Tuple
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
    n =
      Unsafe.unsafePartial (Maybe.fromJust (Int.fromString
        (Unsafe.unsafePartial (ArrayPartial.head lines))))
    as =
      map
        (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines)))
    sorted = Array.sortBy (flip compare) as
    indexed = Array.mapWithIndex Tuple.Tuple sorted
    { yes: a, no: b } = Array.partition (Int.even <<< Tuple.fst) indexed
    sum = (Foldable.sum (map Tuple.snd a)) - (Foldable.sum (map Tuple.snd b))
  in
    (show sum) <> "\n"
