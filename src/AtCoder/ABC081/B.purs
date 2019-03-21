module AtCoder.ABC081.B
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
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
  { asss, ns } <-
    case String.split (String.Pattern "\n") (String.trim input) of
      [ns, asss] -> Right { asss, ns }
      _ -> Left "invalid lines"
  ass <- pure (String.split (String.Pattern " ") asss)
  _ <- Either.note "n" (Int.fromString ns)
  as <- Either.note "as" (Traversable.traverse Int.fromString ass)
  pure (show (solve'' as))

solve'' :: Array Int -> Int
solve'' xs = MonadRec.tailRec go { n: 0, xs }
  where
    go { n, xs: xs' } =
      case f xs' of
        Nothing -> MonadRec.Done n
        Just xs'' -> MonadRec.Loop { n: n + 1, xs: xs'' }

f :: Array Int -> Maybe (Array Int)
f = Array.foldRecM go mempty
  where
    go :: Array Int -> Int -> Maybe (Array Int)
    go xs x
      | Int.odd x = Nothing
      | otherwise = Just (Array.snoc xs (x / 2))
