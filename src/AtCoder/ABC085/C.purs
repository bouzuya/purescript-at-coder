module AtCoder.ABC085.C
  ( solve
  , solve''
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  { ns, ys } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [ns, ys] -> Right { ns, ys }
      _ -> Left "invalid input"
  n <- Either.note "n" (Int.fromString ns)
  y <- Either.note "y" (Int.fromString ys)
  pure
    case solve'' n y of
      Just { x, y: y', z } -> Array.intercalate " " (map show [x, y', z])
      Nothing -> "-1 -1 -1"

solve'' :: Int -> Int -> Maybe { x :: Int, y :: Int, z :: Int }
solve'' n y = MonadRec.tailRec go n
  where
    go x'
      | x' < 0 = MonadRec.Done Nothing
      | otherwise =
          case solve''' (n - x') (y - (10000 * x')) of
            Just { y: y', z: z' } ->
              MonadRec.Done (Just { x: x', y: y', z: z' })
            Nothing -> MonadRec.Loop (x' - 1)

solve''' :: Int -> Int -> Maybe { y :: Int, z :: Int }
solve''' n y = MonadRec.tailRec go n
  where
    go y'
      | y' < 0 = MonadRec.Done Nothing
      | (5000 * y' + 1000 * (n - y')) == y =
        MonadRec.Done (Just { y: y', z: n - y' })
      | otherwise = MonadRec.Loop (y' - 1)
