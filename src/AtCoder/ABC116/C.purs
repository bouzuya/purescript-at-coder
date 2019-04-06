module AtCoder.ABC116.C
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
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  hs <- Either.note "hs" do
    hsLine <- Array.last lines
    pure
      (Array.mapMaybe Int.fromString (String.split (String.Pattern " ") hsLine))
  pure (show (solve'' n hs))

solve'' :: Int -> Array Int -> Int
solve'' n hs = ST.run do
  countRef <- STRef.new 0
  continueRef <- STRef.new false
  updatedRef <- STRef.new true
  hsta <- STArray.unsafeThaw hs
  ST.while (STRef.read updatedRef) do
    void (STRef.write false continueRef)
    void (STRef.write false updatedRef)
    ST.for 0 n \i -> do
      hMaybe <- STArray.peek i hsta
      h <- Maybe.maybe' (\_ -> Unsafe.unsafeCrashWith "hMaybe") pure hMaybe
      if h > 0
        then do
          continue <- STRef.read continueRef
          when (not continue) do
            void (STRef.modify (add 1) countRef)
            void (STRef.write true continueRef)
            void (STRef.write true updatedRef)
          void (STArray.poke i (h - 1) hsta)
        else
          void (STRef.write false continueRef)
  STRef.read countRef
