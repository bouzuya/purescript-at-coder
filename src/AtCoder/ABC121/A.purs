module AtCoder.ABC121.A
  ( solve
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.Traversable as Traversable
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  lines <-
    Traversable.traverse
      (\line ->
        case String.split (String.Pattern " ") line of
          [hs, ws] -> do
            h <- Either.note "h" (Int.fromString hs)
            w <- Either.note "w" (Int.fromString ws)
            pure { h, w }
          _ -> Left "invalid line")
      (String.split (String.Pattern "\n") (String.trim input))
  { hw, hw' } <-
    case lines of
      [hw, hw'] -> Right { hw, hw' }
      _ -> Left "invalid lines"
  pure (show (solve'' hw hw'))

solve'' :: { h :: Int, w :: Int } -> { h :: Int, w :: Int } -> Int
solve'' { h, w } { h: h', w: w' } = (h - h') * (w - w')
