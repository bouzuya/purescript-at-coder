-- abc126_b
module ABC126B
  ( main
  , solve
  ) where

import Prelude

import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
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
  let s = String.trim input
  pure ((solve' s) <> "\n")

solve' :: String -> String
solve' s =
  let
    toInt s' = Unsafe.unsafePartial (Maybe.fromJust (Int.fromString s'))
    n1 = toInt (CodeUnits.take 2 s)
    n2 = toInt (CodeUnits.drop 2 s)
    isYYMM = (1 <= n2 && n2 <= 12)
    isMMYY = (1 <= n1 && n1 <= 12)
  in
    case isYYMM, isMMYY of
      true, true -> "AMBIGUOUS"
      true, false -> "YYMM"
      false, true -> "MMYY"
      false, false -> "NA"
