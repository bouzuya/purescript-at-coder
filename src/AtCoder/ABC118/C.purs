module AtCoder.ABC118.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class (Step)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  ns <- Either.note "ns" (Array.head lines)
  n <- Either.note "n" (Int.fromString ns)
  ass <- Either.note "ass" (Array.head (Array.drop 1 lines))
  let as = Array.mapMaybe Int.fromString (String.split (String.Pattern " ") ass)
  pure (show (g as))

g :: Array Int -> Int
g xs = ST.run do
    sta <- STArray.unsafeThaw xs
    MonadRec.tailRecM go sta
  where
    go :: forall h. STArray h Int -> ST h (Step (STArray h Int) Int)
    go sta = do
      a <- STArray.unsafeFreeze sta
      case a of
        [t] -> pure (MonadRec.Done t)
        _ -> do
          (Tuple index v) <-
            pure
              (Array.foldl
                (\ta@(Tuple _ ia) tb@(Tuple _ ib) -> if ia < ib then tb else ta)
                bottom
                (Array.mapWithIndex Tuple a))
          _ <- STArray.splice index 1 [] sta
          os <- STArray.unsafeFreeze sta
          let
            m =
              Array.foldl
                (\acc o ->
                  case acc, (v `mod` o) of
                    Nothing, 0 -> Nothing
                    Nothing, m' -> Just m'
                    Just acc', 0 -> Just acc'
                    Just acc', m' -> Just (min acc' m'))
                Nothing
                os
          case m of
            Nothing -> pure unit
            Just m' -> void (STArray.push m' sta)
          pure (MonadRec.Loop sta)
