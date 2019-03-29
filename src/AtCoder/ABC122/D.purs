module AtCoder.ABC122.D
  ( solve
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve'' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve'' :: String -> Either String String
solve'' input = do
  n <- Either.note "N" (Int.fromString (String.trim input))
  pure (show (f n))

f :: Int -> Int
f n = ST.run do
  memo <- STArray.unsafeThaw (Array.replicate n Object.empty)
  dfs memo 0 "TTT"
  where
    dfs :: forall r. STArray r (Object Int) -> Int -> String -> ST r Int
    dfs memo cur last3 = do
      oMaybe <- STArray.peek cur memo
      o <- pure (Maybe.fromMaybe Object.empty oMaybe)
      case Object.lookup last3 o of
        Just v -> pure v
        Nothing ->
          if cur == n
            then pure 1
            else do
              ref <- STRef.new 0
              ST.foreach ["A", "C", "G", "T"] \c -> do
                let s' = last3 <> c
                if ok s'
                  then do
                    v' <- dfs memo (cur + 1) (CodeUnits.drop 1 s')
                    void (STRef.modify ((flip mod 1000000007) <<< (add v')) ref)
                  else pure unit
              v' <- STRef.read ref
              void (STArray.poke cur (Object.insert last3 v' o) memo)
              pure v'

    ok :: String -> Boolean
    ok s =
      let
        cs = CodeUnits.toCharArray s
        index = Unsafe.unsafePartial (Array.unsafeIndex cs)
        ss =
          map
            (CodeUnits.fromCharArray <<< (map index))
            [ [0, 1, 2, 3]
            , [1, 0, 2, 3]
            , [0, 2, 1, 3]
            , [0, 1, 3, 2]
            ]
      in
        Array.all
          (Maybe.isNothing <<< (CodeUnits.indexOf (String.Pattern "AGC")))
          ss
