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
  _ <- Either.note "N" ((Array.head lines) >>= Int.fromString)
  plans <-
    Array.foldRecM
      (\a txy ->
        case String.split (String.Pattern " ") txy of
          [ts, xs, ys] -> do
            t <- Either.note "t" (Int.fromString ts)
            x <- Either.note "x" (Int.fromString xs)
            y <- Either.note "y" (Int.fromString ys)
            pure (Array.snoc a { t, x, y })
          _ -> Left ("invalid line: " <> txy))
      []
      (Array.drop 1 lines)
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
            dx = x' - x
            dy = y' - y
          in
            if ((Ord.abs dx) + (Ord.abs dy) > dt) ||
              (Int.odd (dt - ((Ord.abs dx) + (Ord.abs dy))))
              then
                MonadRec.Done false
              else
                MonadRec.Loop
                  { curr: { t: t', x: x', y: y' }, plans: Array.drop 1 plans }
