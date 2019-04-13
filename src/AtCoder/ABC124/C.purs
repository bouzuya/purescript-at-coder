module AtCoder.ABC124.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let s = String.split (String.Pattern "") (String.trim input)
  pure (show (solve'' s))

solve'' :: Array String -> Int
solve'' cs = MonadRec.tailRec go { i: 0, e: 0, o: 0 }
  where
    l = Array.length cs
    -- e: 010..., o: 101...
    -- 000
    go { i, e, o } =
      case Array.index cs i of
        Maybe.Nothing -> MonadRec.Done (min e o)
        Maybe.Just c ->
          MonadRec.Loop
            { i: i + 1
            , e: e +
                if (Int.even i && c == "0") || (Int.odd i && c == "1")
                  then 0
                  else 1
            , o: o +
                if (Int.even i && c == "1") || (Int.odd i && c == "0")
                  then 0
                  else 1
            }


