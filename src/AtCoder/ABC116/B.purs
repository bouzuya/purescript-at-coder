module AtCoder.ABC116.B
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
  s <- Either.note "s" (Int.fromString (String.trim input))
  pure (show (solve'' s))

solve'' :: Int -> Int
solve'' s = ST.run do
  sta <- STArray.empty
  _ <- STArray.push s sta
  MonadRec.tailRecM go sta
  where
    go :: forall h. STArray h Int -> ST h (Step (STArray h Int) Int)
    go sta = do
      a <- STArray.unsafeFreeze sta
      prevMaybe <- STArray.peek ((Array.length a) - 1) sta
      prev <- Maybe.maybe' (\_ -> Unsafe.unsafeCrashWith "prev") pure prevMaybe
      let am = f prev
      if Maybe.isJust (Array.findIndex (eq am) a)
        then pure (MonadRec.Done ((Array.length a) + 1))
        else do
          _ <- STArray.push am sta
          pure (MonadRec.Loop sta)

f :: Int -> Int
f n
  | Int.even n = n / 2
  | otherwise = 3 * n + 1

