module AtCoder.ABC121.D
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  let
    line = String.split (String.Pattern " ") (String.trim input)
    { a, b } =
      case map BigInt.fromString line of
        [Just a, Just b] -> { a, b }
        _ -> Unsafe.unsafeCrashWith "BigInt.fromString"
  in (BigInt.toString (solve'' a b)) <> "\n"

solve'' :: BigInt -> BigInt -> BigInt
solve'' a b = MonadRec.tailRec go { i: a, r: a }
  where
    go { i, r }
      | i == b = MonadRec.Done r
      | otherwise =
        let i' = i + one
        in MonadRec.Loop { i: i', r: BigInt.xor i' r }
