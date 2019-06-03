-- abc093_b
module ABC093B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { a, b, k } <-
    case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
      [a, b, k] -> Either.Right { a, b, k }
      _ -> Either.Left "a b k"
  pure ((String.joinWith "\n" (map show (solve' a b k))) <> "\n")

solve' :: Int -> Int -> Int -> Array Int
solve' a b k =
  let
    xs = Array.range a (min (a + k - 1) b)
    ys = Array.range (max a (b - k + 1)) b
  in
    Array.sort (Array.nub (xs <> ys))
