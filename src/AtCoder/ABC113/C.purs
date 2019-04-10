module AtCoder.ABC113.C
  ( solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  { n, m } <-
    Either.note
      "n m"
      do
        nmLine <- Array.head lines
        case
          (Array.mapMaybe
            Int.fromString
            (String.split (String.Pattern " ") nmLine)) of
          [n, m] -> Maybe.Just { n, m }
          _ -> Maybe.Nothing
  let
    pys = ST.run do
      sta <- STArray.empty
      ST.for 0 m \i -> do
        case
          Array.mapMaybe
            Int.fromString
            (String.split
              (String.Pattern " ")
              (Unsafe.unsafePartial (Array.unsafeIndex lines (i + 1)))) of
          [p, y] -> void (STArray.push { p, y, i } sta)
          _ -> Unsafe.unsafeCrashWith "p y line"
      _ <-
        STArray.sortBy
          (\{ y: y1, p: p1 } { y: y2, p: p2 } ->
            case compare p1 p2 of
              LT -> LT
              GT -> GT
              EQ -> compare y1 y2)
          sta
      STArray.unsafeFreeze sta
    cs = ST.run do
      sta <- STArray.unsafeThaw (Array.replicate m "")
      pRef <- STRef.new 0
      xRef <- STRef.new 0
      ST.foreach pys \{ p, i } -> do
        p' <- STRef.read pRef
        x <- STRef.read xRef
        let x' = (if p' /= p then 0 else x) + 1
        _ <- STArray.poke i (pad p <> pad x') sta
        _ <- STRef.write p pRef
        _ <- STRef.write x' xRef
        pure unit
      STArray.unsafeFreeze sta
  pure (String.joinWith "\n" cs)

pad :: Int -> String
pad x =
  let
    s = show x
  in
    case CodeUnits.length s of
      0 -> "000000"
      1 -> "00000" <> s
      2 -> "0000" <> s
      3 -> "000" <> s
      4 -> "00" <> s
      5 -> "0" <> s
      _ -> s
