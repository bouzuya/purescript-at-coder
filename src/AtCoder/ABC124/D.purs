module AtCoder.ABC124.D
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.ST as STArray
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  Tuple.Tuple n k <-
    Either.note "n k" do
      nkLine <- Array.head lines
      case
        Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") nkLine) of
        [n, k] -> Maybe.Just (Tuple.Tuple n k)
        _ -> Maybe.Nothing
  cs <-
    Either.note "cs" do
      csLine <- Array.last lines
      pure
        (map (eq "1") (String.split (String.Pattern "") csLine))
  pure (show (solve'' n k cs))

solve'' :: Int -> Int -> Array Boolean -> Int
solve'' n k cs
  | k >= n = n
  | otherwise = -- k < n
      let
        grouped =
          map
            (\a -> Tuple.Tuple (NonEmptyArray.head a) (NonEmptyArray.length a))
            (Array.group cs)
        c = Tuple.fst (Unsafe.unsafePartial (Array.unsafeIndex grouped 0))
        cs' = Array.cons (Tuple.Tuple (not c) 0) (f grouped)
        length = Array.length cs'
      in
        MonadRec.tailRec
          (\(Tuple.Tuple i acc) ->
            if i >= length
              then MonadRec.Done acc
              else
                let
                  Tuple.Tuple av an =
                    Unsafe.unsafePartial (Array.unsafeIndex cs' i)
                  bi = min (length - 1) (i + k * 2 + if av then 0 else 1)
                  Tuple.Tuple bv bn =
                    Unsafe.unsafePartial (Array.unsafeIndex cs' bi)
                in
                  MonadRec.Loop
                    (Tuple.Tuple (i + 1) (max acc (bn - an))))
          (Tuple.Tuple 0 0)
    where
      f a = ST.run do
        sta <- STArray.empty
        cRef <- STRef.new 0
        ST.foreach a \(Tuple.Tuple b l) -> do
          c <- STRef.read cRef
          _ <- STArray.push (Tuple.Tuple b (c + l)) sta
          _ <- STRef.write (c + l) cRef
          pure unit
        STArray.unsafeFreeze sta
