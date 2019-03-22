module AtCoder.ABC085.C
  ( solve
  , solve''
  ) where

import Prelude

import Control.MonadZero as MonadZero
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
  { ns, ys } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [ns, ys] -> Right { ns, ys }
      _ -> Left "invalid input"
  n <- Either.note "n" (Int.fromString ns)
  y <- Either.note "y" (Int.fromString ys)
  pure
    case Array.head (solve'' n y) of
      Just { x, y: y', z } -> Array.intercalate " " (map show [x, y', z])
      Nothing -> "-1 -1 -1"

solve'' :: Int -> Int -> Array { x :: Int, y :: Int, z :: Int }
solve'' n y = do
  x' <- Array.range n 0
  MonadZero.guard (10000 * x' <= y)
  y' <- Array.range (n - x') 0
  MonadZero.guard (10000 * x' + 5000 * y' <= y)
  let z' = n - x' - y'
  MonadZero.guard (10000 * x' + 5000 * y' + 1000 * z' == y)
  pure { x: x', y: y', z: z' }

