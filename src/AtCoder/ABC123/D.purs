module AtCoder.ABC123.D
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class (Step)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

split :: String -> Array String
split = String.split (String.Pattern " ")

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { x, y, z, k, as, bs, cs } <-
    case String.split (String.Pattern "\n") (String.trim input) of
      [xyzkLine, aLine, bLine, cLine] -> do
        { x, y, z, k } <-
          case Array.mapMaybe Int.fromString (split xyzkLine) of
            [x, y, z, k] -> pure { x, y, z, k }
            _ -> Either.Left "x y z k"
        let
          as = Array.mapMaybe BigInt.fromString (split aLine)
          bs = Array.mapMaybe BigInt.fromString (split bLine)
          cs = Array.mapMaybe BigInt.fromString (split cLine)
        pure { x, y, z, k, as, bs, cs }
      _ -> Either.Left "lines"
  pure (String.joinWith "\n" (solve' x y z k as bs cs))

data Cake = Cake Int Int Int BigInt
derive instance eqCake :: Eq Cake
instance ordCake :: Ord Cake where
  compare
    (Cake ai bi ci xyz)
    (Cake ai' bi' ci' xyz') =
    case compare xyz xyz' of
      LT -> LT
      GT -> GT
      EQ -> case compare ai ai' of
        LT -> LT
        GT -> GT
        EQ -> case compare bi bi' of
          LT -> LT
          GT -> GT
          EQ -> compare ci ci'

type Q a = Tuple (Set a) (Set a)

type A h = { ki :: Int, qRef :: STRef h (Q Cake), sta :: STArray h String }

solve' ::
  Int
  -> Int
  -> Int
  -> Int
  -> Array BigInt
  -> Array BigInt
  -> Array BigInt
  -> Array String
solve' x y z k as bs cs = ST.run do
    sta <- STArray.empty
    qRef <- STRef.new (enq (cake 0 0 0) (Tuple.Tuple Set.empty Set.empty))
    MonadRec.tailRecM go { ki: min k (x * y * z), qRef, sta }
  where
    as' = Array.sortBy (flip compare) as
    bs' = Array.sortBy (flip compare) bs
    cs' = Array.sortBy (flip compare) cs

    cake :: Int -> Int -> Int -> Maybe Cake
    cake ai bi ci = do
      a <- Array.index as' ai
      b <- Array.index bs' bi
      c <- Array.index cs' ci
      pure (Cake ai bi ci (a + b + c))

    enq :: Maybe Cake -> Q Cake -> Q Cake
    enq Maybe.Nothing q = q
    enq (Maybe.Just c) q'@(Tuple.Tuple s q)
      | Set.member c s = q'
      | otherwise = Tuple.Tuple (Set.insert c s) (Set.insert c q)

    go :: forall h. A h -> ST h (Step (A h) (Array String))
    go { ki, qRef, sta }
      | ki == 0 = do
        a <- STArray.unsafeFreeze sta
        pure (MonadRec.Done a)
      | otherwise = do
        Tuple.Tuple s q <- STRef.read qRef
        let
          m = Unsafe.unsafePartial (Maybe.fromJust (Set.findMax q))
          q' = Tuple.Tuple s (Set.delete m q)
          (Cake ai bi ci xyz) = m
          qa = enq (cake (ai + 1) bi ci) q'
          qb = enq (cake ai (bi + 1) ci) qa
          qc = enq (cake ai bi (ci + 1)) qb
        _ <- STArray.push (BigInt.toString xyz) sta
        _ <- STRef.write qc qRef
        pure (MonadRec.Loop { ki: ki - 1, qRef, sta })
