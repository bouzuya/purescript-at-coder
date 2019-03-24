module AtCoder.ABC121.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.HugeInt (HugeInt)
import Data.HugeInt as HugeInt
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  { abss, nms } <- do
    nms <- Either.note "nms" (Array.head lines)
    abss <- pure (Array.drop 1 lines)
    pure { abss, nms }
  { m, n } <-
    case String.split (String.Pattern " ") nms of
      [ns, ms] -> do
        n <- Either.note "n" (Int.fromString ns)
        m <- Either.note "m" (Int.fromString ms)
        pure { m, n }
      _ -> Left "n m"
  abs <-
    pure
      (Array.mapMaybe
        (\i ->
          case String.split (String.Pattern " ") i of
            [as, bs] -> do
              a <- HugeInt.fromString as
              b <- Int.fromString bs
              pure (Tuple a b)
            _ -> Nothing)
        abss)
  if Array.length abs == n then pure unit else Left "abs length"
  Either.note
    "HugeInt"
    (String.stripPrefix (String.Pattern "HugeInt ") (show (solve'' m abs)))

solve'' :: Int -> Array (Tuple HugeInt Int) -> HugeInt
solve'' m abs = MonadRec.tailRec go { i: 0, m': m, n: HugeInt.fromInt 0 }
  where
    sorted = Array.sortWith Tuple.fst abs
    go { i, m', n } =
      case Array.index sorted i of
        Nothing -> MonadRec.Done n
        Just (Tuple a b) ->
          let m'' = m' - b
          in
            if m'' < 0
              then
                MonadRec.Done
                  (n + (a * (HugeInt.fromInt m')))
              else
                MonadRec.Loop
                  { i: i + 1, m': m'', n: (n + (a * (HugeInt.fromInt b))) }
