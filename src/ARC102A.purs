-- arc102_a or abc108_c
module ARC102A
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  Tuple.Tuple n k <-
    case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
      [n, k] -> Either.Right (Tuple.Tuple n k)
      _ -> Either.Left "n k"
  pure ((show (solve' n k)) <> "\n")

solve' :: Int -> Int -> Int
solve' n k
  | Int.odd k =
    Int.pow
      (Array.length
        (Array.filter
          (\x -> x `mod` k == 0)
          (Array.range 1 n)))
      3
  | otherwise =
    (Int.pow
      (Array.length
        (Array.filter
          (\x -> x `mod` k == 0)
          (Array.range 1 n)))
      3) +
    (Int.pow
      (Array.length
        (Array.filter
          (\x -> x `mod` k == (k / 2))
          (Array.range 1 n)))
      3)
