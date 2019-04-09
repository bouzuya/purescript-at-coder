module AtCoder.ABC114.B
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let s = String.trim input
  pure (show (solve'' s))

solve'' :: String -> Int
solve'' s = MonadRec.tailRec go { i: 0, r: top }
  where
    go { i, r } =
      case (CodeUnits.slice i (i + 3) s) >>= Int.fromString of
        Maybe.Nothing -> MonadRec.Done r
        Maybe.Just x ->
          MonadRec.Loop
            { i: i + 1
            , r: min r (Ord.abs (x - 753))
            }
