module AtCoder.ABC112.C
  ( solve
  ) where

import Prelude

import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" (Array.head lines >>= Int.fromString)
  let
    hs =
      Array.mapMaybe
        (\line ->
          case
            Array.mapMaybe
              Int.fromString
              (String.split (String.Pattern " ") line) of
            [x, y, h] -> Maybe.Just { x, y, h }
            _ -> Maybe.Nothing)
        (Array.drop 1 lines)
  { x: cx, y: cy, h: ch } <- Either.note "c" (solve'' n hs)
  pure (String.joinWith " " (map show [cx, cy, ch]))

type P = { x :: Int, y :: Int, h :: Int }

solve'' :: Int -> Array P -> Maybe P
solve'' n hs = do
  { x: x', y: y', h: h' } <- Array.find (\{ h } -> h >= one) hs
  let
    fh cx cy = h' + Ord.abs (x' - cx) + Ord.abs (y' - cy)
    cs = do
      cx <- Array.range 0 100
      cy <- Array.range 0 100
      ch <- pure (fh cx cy)
      MonadZero.guard (ch >= one)
      pure { x: cx, y: cy, h: ch }
    ok { x: cx, y: cy, h: ch } =
      (Array.length
        (Array.filter
          (\{ x, y, h } ->
            (max (ch - Ord.abs (x - cx) - Ord.abs (y - cy)) zero) == h)
          hs)) == n
  Array.find ok cs
