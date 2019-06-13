-- abc089_b
module ABC089B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array.Partial as ArrayPartial
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Foreign.Object as Object
import Foreign.Object.ST as STObject
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
    ss = splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines))
    o = ST.run do
      sto <- STObject.new
      ST.foreach ss \s -> do
        void (STObject.poke s unit sto)
      Object.freezeST sto
  in
    case Object.size o of
      3 -> "Three\n"
      4 -> "Four\n"
      _ -> Unsafe.unsafeCrashWith "invalid"
