-- abc129_b
module ABC129B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Array.Partial as ArrayPartial
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
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
  n <-
    Either.note "n"
      (Int.fromString (Unsafe.unsafePartial (ArrayPartial.head lines)))
  let
    ws =
      Array.mapMaybe
        Int.fromString
        (splitBySP (Unsafe.unsafePartial (ArrayPartial.last lines)))
  pure ((show (solve' n ws)) <> "\n")

solve' :: Int -> Array Int -> Int
solve' n ws =
  let sum = Foldable.sum ws
  in
    Tuple.snd
      (Array.foldl
        (\(Tuple.Tuple sum' min') w ->
          (Tuple.Tuple
            (sum' + w)
            (min min' (Ord.abs ((sum' + w) - (sum - (sum' + w)))))))
        (Tuple.Tuple 0 sum)
        ws)
