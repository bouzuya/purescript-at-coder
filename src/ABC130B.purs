-- abc130_b
module ABC130B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
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

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP = String.split (String.Pattern " ")

solve :: String -> String
solve input =
  let
    lines = splitByNL input
    nx =
      map
        (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.head lines)))
    n = Unsafe.unsafePartial (ArrayPartial.head nx)
    x = Unsafe.unsafePartial (ArrayPartial.last nx)
    ls =
      map
        (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines)))
    go { d, i }
      | i == n = MonadRec.Done (i + 1)
      | otherwise =
        let l = Unsafe.unsafePartial (Array.unsafeIndex ls i)
        in
          if d + l <= x
            then MonadRec.Loop { d: d + l, i: i + 1 }
            else MonadRec.Done (i + 1)
  in
    (show (MonadRec.tailRec go { d: 0, i: 0 })) <> "\n"
