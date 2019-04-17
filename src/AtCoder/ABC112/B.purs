module AtCoder.ABC112.B
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

n2 :: String -> Maybe (Tuple Int Int)
n2 s =
  case
    Array.mapMaybe
      Int.fromString
      (String.split (String.Pattern " ") s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  Tuple.Tuple n t <- Either.note "n t" do
    line <- Array.head lines
    n2 line
  let cts = Array.mapMaybe n2 (Array.drop 1 lines)
  pure
    case solve'' n t cts of
      Maybe.Just cost -> show cost
      Maybe.Nothing -> "TLE"

solve'' :: Int -> Int -> Array (Tuple Int Int) -> Maybe Int
solve'' n t cts = MonadRec.tailRec go { cost: Maybe.Nothing, i: 0 }
  where
    go { cost, i } =
      case Array.index cts i of
        Maybe.Nothing -> MonadRec.Done cost
        Maybe.Just (Tuple.Tuple c' t') ->
          MonadRec.Loop
            { cost:
              if t' > t
                then cost
                else Maybe.Just (Maybe.maybe c' (min c') cost)
            , i: i + 1
            }
