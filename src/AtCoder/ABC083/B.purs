module AtCoder.ABC083.B
  ( solve
  ) where

import Prelude

import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  { as, bs, ns } <-
    case String.split (String.Pattern " ") (String.trim input) of
      [ns, as, bs] -> Right { as, bs, ns }
      _ -> Left "invalid line"
  a <- Either.note "a" (Int.fromString as)
  b <- Either.note "b" (Int.fromString bs)
  n <- Either.note "n" (Int.fromString ns)
  pure (show (Foldable.sum (solve'' n a b)))

solve'' :: Int -> Int -> Int -> Array Int
solve'' n a b = do
  n' <- Array.range 1 n
  let
    sum =
      Foldable.sum
        (map
          (\s -> Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s)))
          (String.split (String.Pattern "") (show n')))
  MonadZero.guard (between a b sum)
  pure n'
