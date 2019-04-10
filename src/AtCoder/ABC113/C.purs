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
    pys :: Array { p :: Int, y :: Int, i :: Int }
    pys =
      Array.mapWithIndex
        (\i { p, y } -> { p, y, i: i + 1 })
        (Array.mapMaybe
          (\pyLine ->
            case
              (Array.mapMaybe
                Int.fromString
                (String.split (String.Pattern " ") pyLine)) of
              [p, y] -> Maybe.Just { p, y }
              _ -> Maybe.Nothing)
          (Array.drop 1 lines))
  pure (String.joinWith "\n" (solve'' n m pys))

solve'' :: Int -> Int -> Array { p :: Int, y :: Int, i :: Int } -> Array String
solve'' n _ pys = ST.run do
  let
    cs :: Array { y :: Int, p :: Int, i :: Int }
    cs =
      Array.sortBy
        (\{ y: y1, p: p1 } { y: y2, p: p2 } ->
          case compare p1 p2 of
            LT -> LT
            GT -> GT
            EQ -> compare y1 y2)
        pys
  sta <- STArray.empty
  _ <-
    Array.foldRecM
      (\{ p', x } { p, i } -> do
        let x' = (if p' /= p then 0 else x) + 1
        _ <- STArray.push { p, x: x', i } sta
        pure { p': p, x: x' })
      { p': -1, x: 0 }
      cs
  _ <- STArray.sortWith _.i sta
  cs' <- STArray.unsafeFreeze sta
  pure (map (\{ p, x } -> pad p <> pad x) cs')

  where
    pad :: Int -> String
    pad m =
      let
        s = show m
        l = String.length s
      in
        if l < 6
          then (String.joinWith "" (Array.replicate (6 - l) "0")) <> s
          else s
