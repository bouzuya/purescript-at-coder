module AtCoder.ABC086.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  lines <- pure (String.split (String.Pattern "\n") (String.trim input))
  n <- Either.note "N" ((Array.head lines) >>= Int.fromString)
  plans <-
    pure
      (Array.mapMaybe
        (\txy ->
          case String.split (String.Pattern " ") txy of
            [ts, xs, ys] -> do
              t <- Int.fromString ts
              x <- Int.fromString xs
              y <- Int.fromString ys
              pure { t, x, y }
            _ -> Nothing)
        (Array.drop 1 lines))
  if (Array.length plans) /= n then Left "invalid lines" else pure unit
  pure (solve'' plans)

solve'' :: Array { t :: Int, x :: Int, y :: Int } -> String
solve'' plans
  | solve''' plans = "Yes"
  | otherwise = "No"

solve''' :: Array { t :: Int, x :: Int, y :: Int } -> Boolean
solve''' = (MonadRec.tailRec go) <<< ({ curr: { t: 0, x: 0, y: 0 }, plans: _ })
  where
    go { curr: { t, x, y }, plans } =
      case Array.head plans of
        Nothing -> MonadRec.Done true
        Just { t: t', x: x', y: y' } ->
          let
            dt = t' - t
            dx = Ord.abs (x' - x)
            dy = Ord.abs (y' - y)
          in
            if (dx + dy > dt) || (Int.odd (dt - (dx + dy)))
              then MonadRec.Done false
              else
                MonadRec.Loop
                  { curr: { t: t', x: x', y: y' }, plans: Array.drop 1 plans }
