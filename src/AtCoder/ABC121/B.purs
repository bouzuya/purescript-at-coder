module AtCoder.ABC121.B
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable as Traversable
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  { asss, bss, nmc } <- do
    nmc <- Either.note "nmc" (Array.head lines)
    bss <- Either.note "bss" (Array.head (Array.drop 1 lines))
    asss <- pure (Array.drop 2 lines)
    pure { asss, bss, nmc }
  { c, m, n } <-
    case String.split (String.Pattern " ") nmc of
      [ns, ms, cs] -> do
        n <- Either.note "n" (Int.fromString ns)
        m <- Either.note "m" (Int.fromString ms)
        c <- Either.note "c" (Int.fromString cs)
        pure { c, m, n }
      _ -> Left "n m c"
  bs <-
    pure (Array.mapMaybe Int.fromString (String.split (String.Pattern " ") bss))
  if Array.length bs /= m then Left "bs length" else pure unit
  ass <-
    pure
      (map
        (\i ->
          Array.mapMaybe Int.fromString (String.split (String.Pattern " ") i))
        asss)
  if Array.length ass == n then pure unit else Left "ass length"
  if Array.all ((eq m) <<< Array.length) ass
    then pure unit
    else Left "as length"
  pure (show (solve'' c bs ass))

solve'' :: Int -> Array Int -> Array (Array Int) -> Int
solve'' c bs ass = MonadRec.tailRec go { i: 0, n: 0 }
  where
    go { i, n } =
      case Array.index ass i of
        Nothing -> MonadRec.Done n
        Just as ->
          if (Traversable.sum (Array.zipWith (*) as bs)) + c > 0
            then MonadRec.Loop { i: i + 1, n: n + 1 }
            else MonadRec.Loop { i: i + 1, n }
