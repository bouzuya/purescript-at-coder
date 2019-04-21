module AtCoder.ABC111.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  vs <- Either.note "vs" do
    line <- Array.last lines
    pure (Array.mapMaybe Int.fromString (splitBySP line))
  -- 奇数番目・偶数番目それぞれですべて同一にしないといけない
  pure (show (solve' n vs))

solve' :: Int -> Array Int -> Int
solve' n vs = ST.run do
  oddRef <- STRef.new true
  oddCountSTA <- STArray.unsafeThaw (Array.replicate (Int.pow 10 5) 0)
  evenCountSTA <- STArray.unsafeThaw (Array.replicate (Int.pow 10 5) 0)
  ST.foreach vs \v -> do
    isOdd <- STRef.read oddRef
    let countSTA = if isOdd then oddCountSTA else evenCountSTA
    _ <- STArray.modify v (add 1) countSTA
    _ <- STRef.write (not isOdd) oddRef
    pure unit
  oddCounts <- STArray.unsafeFreeze oddCountSTA
  evenCounts <- STArray.unsafeFreeze evenCountSTA
  let
    Tuple.Tuple max1odd max2odd = takeMax2 oddCounts
    Tuple.Tuple max1even max2even = takeMax2 evenCounts
    f :: Maybe Int -> Maybe Int -> Maybe Int
    f o e = do
      i1 <- o
      i2 <- e
      if o == e then Maybe.Nothing else pure unit
      x1 <- Array.index oddCounts i1
      x2 <- Array.index evenCounts i2
      pure (((n / 2) - x1) + ((n / 2) - x2))
    x :: Int
    x =
      Array.foldl
        min
        top
        (Array.mapMaybe
          identity
          [ f max1odd max1even
          , f max1odd max2even
          , f max2odd max1even
          , f max2odd max2even
          ])
  pure x


takeMax2 :: Array Int -> Tuple (Maybe Int) (Maybe Int)
takeMax2 xs =
  MonadRec.tailRec
    go
    { i: ((Array.length xs) - 1)
    , m: Tuple.Tuple Maybe.Nothing Maybe.Nothing
    }
  where
    go { i, m: Tuple.Tuple max1 max2 } =
      case Array.index xs i of
        Maybe.Nothing -> MonadRec.Done (Tuple.Tuple max1 max2)
        Maybe.Just x ->
          MonadRec.Loop
            { i: i - 1
            , m:
              case max1, max2 of
                Maybe.Nothing, Maybe.Nothing ->
                  Tuple.Tuple (Maybe.Just i) Maybe.Nothing
                (Maybe.Just i1), Maybe.Nothing ->
                  let
                    x1 = Unsafe.unsafePartial (Array.unsafeIndex xs i1)
                  in
                    if x > x1
                      then Tuple.Tuple (Maybe.Just i) (Maybe.Just i1)
                      else Tuple.Tuple (Maybe.Just i1) (Maybe.Just i)
                (Maybe.Just i1), (Maybe.Just i2) ->
                  let
                    x1 = Unsafe.unsafePartial (Array.unsafeIndex xs i1)
                    x2 = Unsafe.unsafePartial (Array.unsafeIndex xs i2)
                  in
                    if x > x2
                      then
                        if x > x1
                          then Tuple.Tuple (Maybe.Just i) (Maybe.Just i1)
                          else Tuple.Tuple (Maybe.Just i1) (Maybe.Just i)
                      else
                        Tuple.Tuple (Maybe.Just i1) (Maybe.Just i2)
                _, _ ->
                  Unsafe.unsafeCrashWith "max"
            }
