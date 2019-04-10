module AtCoder.ABC113.C
  ( solve
  ) where

import Prelude

import Control.Monad.ST as ST
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
    pys =
      map
        (\pyLine ->
          case
            (Array.mapMaybe
              Int.fromString
              (String.split (String.Pattern " ") pyLine)) of
            [p, y] -> { p, y }
            _ -> Unsafe.unsafeCrashWith "p y line")
        (Array.drop 1 lines)
  pure (String.joinWith "\n" (solve'' n m pys))

solve'' :: Int -> Int -> Array { p :: Int, y :: Int } -> Array String
solve'' n m pys = ST.run do
  stpys <-
    STArray.unsafeThaw (Array.mapWithIndex (\i { p, y } -> { p, y, i }) pys)
  _ <-
    STArray.sortBy
      (\{ y: y1, p: p1 } { y: y2, p: p2 } ->
        case compare p1 p2 of
          LT -> LT
          GT -> GT
          EQ -> compare y1 y2)
      stpys
  pys' <- STArray.unsafeFreeze stpys
  sta <- STArray.unsafeThaw (Array.replicate m "")
  _ <-
    Array.foldRecM
      (\{ p', x } { p, i } -> do
        let x' = (if p' /= p then 0 else x) + 1
        _ <- STArray.poke i (pad p <> pad x') sta
        pure { p': p, x: x' })
      { p': -1, x: 0 }
      pys'
  STArray.unsafeFreeze sta

  where
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
