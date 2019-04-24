-- abc108_A
module ABC108A
  ( main
  , solve
  ) where

import Prelude

import Control.MonadZero as MonadZero
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  k <- Either.note "k" (Int.fromString (String.trim input))
  pure ((show (solve' k)) <> "\n")

solve' :: Int -> Int
solve' k = Array.length do
  k1 <- Array.range 1 k
  k2 <- Array.range 1 k
  MonadZero.guard (Int.odd k1)
  MonadZero.guard (Int.even k2)
  pure (Tuple.Tuple k1 k2)
