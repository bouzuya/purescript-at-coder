-- abc128_b
module ABC128B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
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

int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { head: nLine, tail: spLines } <-
    Either.note "lines" (Array.uncons (splitByNL input))
  n <- Either.note "n" (Int.fromString nLine)
  let
    sps =
      Array.mapMaybe
        (\spLine -> do
          case splitBySP spLine of
            [s, p] -> do
              p' <- Int.fromString p
              pure (Tuple.Tuple s p')
            _ -> Maybe.Nothing)
        spLines
  pure ((String.joinWith "\n" (map show (solve' n sps))) <> "\n")

solve' :: Int -> Array (Tuple String Int) -> Array Int
solve' a sps =
  let
    sps' :: Array { i :: Int, s :: String, p :: Int }
    sps' = Array.mapWithIndex (\i (Tuple.Tuple s p) -> { i: i + 1, s, p }) sps
    sps'' :: Array { i :: Int, s :: String, p :: Int }
    sps'' =
      Array.sortBy
        (\a1 a2 ->
          case compare a1.s a2.s of
            LT -> LT
            GT -> GT
            EQ -> compare a2.p a1.p)
        sps'
  in
    map _.i sps''


