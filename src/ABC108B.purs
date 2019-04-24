-- abc108_b
module ABC108B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Ref as Ref
import Math as Math
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
  { x1, y1, x2, y2 } <-
    case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
      [x1, y1, x2, y2] -> Either.Right { x1, y1, x2, y2 }
      _ -> Either.Left "x1 y1 x2 y2"
  pure ((String.joinWith " " (map show (solve' x1 y1 x2 y2))) <> "\n")

solve' ::
  Int -> Int -> Int -> Int -> Array Int
solve' x1 y1 x2 y2 =
  let
    dx = x2 - x1
    dy = y2 - y1
    x3 = x2 - dy
    x4 = x1 - dy
    y3 = y2 + dx
    y4 = y1 + dx
  in
    [x3, y3, x4, y4]
