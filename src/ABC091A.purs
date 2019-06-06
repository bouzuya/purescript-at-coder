-- abc091_a
module ABC091A
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

toYesNo :: Boolean -> String
toYesNo yes
  | yes = "Yes"
  | otherwise = "No"

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
    [a, b, c] -> pure ((toYesNo ((a + b) >= c)) <> "\n")
    _ -> Either.Left "a b c"
