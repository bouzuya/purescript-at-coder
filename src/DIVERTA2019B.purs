-- diverta2019_b
module DIVERTA2019B
  ( main
  , solve
  ) where

import Prelude

import Control.MonadZero as MonadZero
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { r, g, b, n } <-
    Either.note "r g b n"
      case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
        [r, g, b, n] -> Maybe.Just { r, g, b, n }
        _ -> Maybe.Nothing
  pure ((show (solve' r g b n)) <> "\n")

solve' :: Int -> Int -> Int -> Int -> Int
solve' r g b n = Array.length do
  r' <- Array.range 0 (min 3000 (n / r))
  MonadZero.guard ((r * r') <= n)
  g' <- Array.range 0 (min 3000 (n / g))
  MonadZero.guard (((r * r') + (g * g')) <= n)
  MonadZero.guard (((n - ((r * r') + (g * g'))) `mod` b) == 0)
  pure unit

