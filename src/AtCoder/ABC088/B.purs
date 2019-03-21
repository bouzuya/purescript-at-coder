module AtCoder.ABC088.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.String as String
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input = Unsafe.unsafePartial (Either.fromRight (solve' input))

solve' :: String -> Either String String
solve' input = do
  { ass, ns } <-
    case String.split (String.Pattern "\n") (String.trim input) of
      [ns, ass] -> Right { ass, ns }
      _ -> Left "invalid line"
  as <-
    Traversable.traverse
      ((Either.note "as") <<< Int.fromString)
      (String.split (String.Pattern " ") ass)
  _ <- Either.note "n" (Int.fromString ns)
  pure (show (solve'' as))

solve'' :: Array Int -> Int
solve'' xs =
  let
    p =
      Array.partition
        (Int.even <<< Tuple.fst)
        (Array.mapWithIndex Tuple (Array.reverse (Array.sort xs)))
    alice = map Tuple.snd p.yes
    bob = map Tuple.snd p.no
  in
    (Traversable.sum alice) - (Traversable.sum bob)
