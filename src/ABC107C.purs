-- abc107_c
module ABC107C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe

main :: Effect Unit
main = Aff.launchAff_ do
  input <- readStdin
  output <- pure (solve input)
  Class.liftEffect (writeStdout output)

readStdin :: Aff.Aff String
readStdin =
  let r = Process.stdin
  in
    Aff.makeAff
      (\callback -> do
        ref <- Ref.new ""
        Stream.onDataString r Encoding.UTF8 \s -> do
          buffer <- Ref.read ref
          Ref.write (buffer <> s) ref
        Stream.onEnd r do
          buffer <- Ref.read ref
          callback (pure buffer)
        pure mempty)

writeStdout :: String -> Effect Unit
writeStdout s =
  void (Stream.writeString Process.stdout Encoding.UTF8 s (pure unit))

splitByNL :: String -> Array String
splitByNL s = String.split (String.Pattern "\n") (String.trim s)

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  nkLine <- Either.note "head" (Array.head lines)
  xsLine <- Either.note "last" (Array.last lines)
  Tuple.Tuple n k <-
    case Array.mapMaybe Int.fromString (splitBySP nkLine) of
      [n, k] -> Either.Right (Tuple.Tuple n k)
      _ -> Either.Left "n k"
  let xs = Array.mapMaybe Int.fromString (splitBySP xsLine)
  pure ((show (solve' n k xs)) <> "\n")

solve' :: Int -> Int -> Array Int -> Int
solve' n k xs = ST.run do
  let
    nxs = Array.reverse (Array.filter (\x -> x < 0) xs)
    pxs = Array.filter (\x -> x > 0) xs
    zx = Maybe.isJust (Array.find (eq 0) xs)
    n' = if zx then n - 1 else n
    k' = if zx then k - 1 else k
  minCostRef <- STRef.new top
  ST.for 0 (k' + 1) \i -> do -- 0 .. k'
    let
      nc' = min (Array.length nxs) (k' - (min (Array.length pxs) i))
      pc' = k' - nc'
      pcost = Maybe.fromMaybe 0 (Array.index pxs (pc' - 1))
      ncost = Ord.abs (Maybe.fromMaybe 0 (Array.index nxs (nc' - 1)))
      cost = if pcost < ncost then pcost * 2 + ncost else pcost + ncost * 2
    STRef.modify (min cost) minCostRef
  STRef.read minCostRef
