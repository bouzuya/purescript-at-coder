-- abc110_b
module ABC110B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
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
  { n, m, x, y, xs, ys } <- Either.note "values" do
    line1 <- Array.index lines 0
    line2 <- Array.index lines 1
    line3 <- Array.index lines 2
    { n, m, x, y } <- case Array.mapMaybe Int.fromString (splitBySP line1) of
      [n, m, x, y] -> Maybe.Just { n, m, x, y }
      _ -> Maybe.Nothing
    let xs = Array.mapMaybe Int.fromString (splitBySP line2)
    let ys = Array.mapMaybe Int.fromString (splitBySP line3)
    pure { n, m, x, y, xs, ys }
  pure ((if solve' n m x y xs ys then "No War" else "War") <> "\n")

solve' :: Int -> Int -> Int -> Int -> Array Int -> Array Int -> Boolean
solve' _ _ x y xs ys =
  let
    maxX = Array.foldl max bottom (Array.cons x xs)
    minY = Array.foldl min top (Array.cons y ys)
  in
    maxX < minY
