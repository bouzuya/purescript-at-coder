module AtCoder.ABC118.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  nmLine <- Either.note "n m line" (Array.head lines)
  { n, m } <-
    case String.split (String.Pattern " ") nmLine of
      [ns, ms] -> do
        n <- Either.note "n" (Int.fromString ns)
        m <- Either.note "m" (Int.fromString ms)
        pure { n, m }
      _ -> Left "n m"
  let kLines = Array.drop 1 lines
  asMaybe <-
    Array.foldRecM
      (\acc kLine -> do
        let
          kas =
            Array.mapMaybe
              Int.fromString
              (String.split (String.Pattern " ") kLine)
        k <- Either.note "k" (Array.head kas)
        let as' = Array.drop 1 kas
        pure (Just (Maybe.maybe as' (Array.intersect as') acc)))
      Nothing
      kLines
  as <- Either.note "as" asMaybe
  pure (show (Array.length as))
