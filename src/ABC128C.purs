-- abc128_c
module ABC128C
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
  let lines = splitByNL input
  nmLine <- Either.note "nmLine" (Array.head lines)
  let ksLines = Array.dropEnd 1 (Array.drop 1 lines)
  pLine <- Either.note "pLine" (Array.last lines)
  Tuple.Tuple n m <- Either.note "n m" (int2 nmLine)
  let
    kss = map ((Array.mapMaybe Int.fromString) <<< splitBySP) ksLines
    ps = Array.mapMaybe Int.fromString (splitBySP pLine)
  pure ((show (solve' n m kss ps)) <> "\n")

solve' :: Int -> Int -> Array (Array Int) -> Array Int -> Int
solve' n m kss ps = Array.length (Array.filter g bss)
  where
    f :: Array Boolean -> Array (Array Boolean)
    f xs
      | Array.length xs == n = [xs]
      | otherwise = (f (Array.snoc xs false)) <> (f (Array.snoc xs true))

    bss :: Array (Array Boolean)
    bss = f []

    g :: Array Boolean -> Boolean
    g bs =
      Array.all
        (\(Tuple.Tuple i kss') ->
          let
            { head: k, tail: ss } =
              Unsafe.unsafePartial (Maybe.fromJust (Array.uncons kss'))
          in
            (eq
              (Unsafe.unsafePartial (Array.unsafeIndex ps i))
              ((Array.length
                (Array.filter
                  (\j -> Unsafe.unsafePartial (Array.unsafeIndex bs (j - 1)))
                  ss)) `mod` 2)))
        (Array.mapWithIndex Tuple.Tuple kss)
