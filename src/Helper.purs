module Helper
  ( binarySearch
  , findMaxIndex
  , int2
  , splitByNL
  , splitBySP
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

binarySearch :: forall a. Ord a => a -> Array a -> Maybe Int
binarySearch x xs = go 0 ((Array.length xs) - 1)
  where
    go l r
      | l > r = Maybe.Nothing
      | otherwise =
        let
          m = l + ((r - l) / 2)
        in
          case compare (pure x) (Array.index xs m) of
            LT -> go l (m - 1)
            EQ -> Maybe.Just m
            GT -> go (m + 1) r

-- | findMaxIndex [] = Nothing
-- | findMaxIndex [1, 3, 2] = Just 1
findMaxIndex :: forall a. Ord a => Array a -> Maybe Int
findMaxIndex xs = do
  headX <- Array.head xs
  pure $ ST.run do
    xRef <- STRef.new headX
    iRef <- STRef.new 0
    ST.for 1 (Array.length xs) \i -> do
      let x = Unsafe.unsafePartial (Array.unsafeIndex xs i)
      x' <- STRef.read xRef
      when (x > x') do
        void (STRef.write x xRef)
        void (STRef.write i iRef)
    STRef.read iRef

-- splitBySP
int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s
