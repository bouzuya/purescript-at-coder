module AtCoder.Tenka12019Beginner.C
  ( solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Partial.Unsafe as Unsafe

-- . is white
-- # is black
solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  s <- Either.note "s" (Array.head (Array.drop 1 lines))
  pure (show (solve' n s))

solve' :: Int -> String -> Int
solve' n s
  | n == 1 = 0
  | otherwise = ST.run do
    bs <- STArray.unsafeThaw (Array.replicate (n + 1) 0)
    ws <- STArray.unsafeThaw (Array.replicate (n + 1) 0)
    bcRef <- STRef.new 0
    wcRef <- STRef.new 0
    ST.for 0 n \i -> do
      bc <- STRef.read bcRef
      wc <- STRef.read wcRef
      let
        isBlack = (CodeUnits.charAt i s) == (Maybe.Just '#')
        bc' = bc + if isBlack then 1 else 0
        wc' = wc + if isBlack then 0 else 1
      _ <- STArray.poke (i + 1) bc' bs
      _ <- STArray.poke (i + 1) wc' ws
      _ <- STRef.write bc' bcRef
      _ <- STRef.write wc' wcRef
      pure unit
    wc <- STRef.read wcRef
    _ <- STArray.poke 0 0 bs
    _ <- STArray.poke 0 0 ws
    minRef <- STRef.new top
    ST.for 0 (n + 1) \i -> do
      m <- STRef.read minRef
      bMaybe <- STArray.peek i bs
      let b = Unsafe.unsafePartial (Maybe.fromJust bMaybe)
      wMaybe <- STArray.peek i ws
      let w = wc - (Unsafe.unsafePartial (Maybe.fromJust wMaybe))
      STRef.write (min (b + w) m) minRef
    STRef.read minRef
