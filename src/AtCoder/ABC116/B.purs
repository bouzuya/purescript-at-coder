module AtCoder.ABC116.B
  ( solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
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
  let length = 1000000
  sta <- STArray.unsafeThaw (Array.replicate length s)
  mRef <- STRef.new 2
  ST.while
    (do
      m <- STRef.read mRef
      if m == length
        then pure false
        else do
          prevMaybe <- STArray.peek (m - 1 - 1) sta
          prev <-
            Maybe.maybe' (\_ -> Unsafe.unsafeCrashWith "prev") pure prevMaybe
          let am = f prev
          _ <- STArray.poke (m - 1) am sta
          a <- STArray.unsafeFreeze sta
          found <-
            pure (Maybe.isJust (Array.find (eq am) (Array.take (m - 1) a)))
          if found
            then pure false
            else do
              _ <- STRef.write (m + 1) mRef
              pure true)
    (pure unit)
  STRef.read mRef

f :: Int -> Int
f n
  | Int.even n = n / 2
  | otherwise = 3 * n + 1

