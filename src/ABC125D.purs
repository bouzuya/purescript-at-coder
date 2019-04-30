-- abc125_d
module ABC125D
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either as Either
import Data.Int as Int
import Data.Ord as Ord
import Data.String as String
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
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  asLine <- Either.note "as" (Array.last lines)
  let as = Array.mapMaybe Int.fromString (splitBySP asLine)
  pure ((BigInt.toString (solve' n as)) <> "\n")

solve' :: Int -> Array Int -> BigInt
solve' n as =
  let
    mc = Array.foldl (\a b -> a + if b < 0 then 1 else 0) 0 as
    as' = map Ord.abs as
  in
    if Int.even mc
      then Array.foldl (\a b -> add a (BigInt.fromInt b)) zero as'
      else
        let
          ma = BigInt.fromInt (Array.foldl min top as')
          sum = Array.foldl (\a b -> add a (BigInt.fromInt b)) zero as'
        in
          sum - (ma * (BigInt.fromInt 2))
