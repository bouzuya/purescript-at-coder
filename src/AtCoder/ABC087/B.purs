module AtCoder.ABC087.B
  ( solve
  ) where

import Prelude

import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  { as, bs, cs, xs } <-
    case String.split (String.Pattern "\n") (String.trim input) of
      [as, bs, cs, xs] -> Right { as, bs, cs, xs }
      _ -> Left "invalid line"
  a <- Either.note "a" (Int.fromString as)
  b <- Either.note "b" (Int.fromString bs)
  c <- Either.note "c" (Int.fromString cs)
  x <- Either.note "x" (Int.fromString xs)
  pure (show (Array.length (solve'' a b c x)))

solve'' :: Int -> Int -> Int -> Int -> Array { a :: Int, b :: Int, c :: Int }
solve'' a b c x = do
  a' <- Array.range 0 a
  MonadZero.guard ((500 * a') <= x)
  b' <- Array.range 0 b
  MonadZero.guard ((500 * a' + 100 * b') <= x)
  c' <- Array.range 0 c
  MonadZero.guard ((500 * a' + 100 * b' + 50 * c') == x)
  pure { a: a', b: b', c: c' }
