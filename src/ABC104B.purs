-- abc104_b
module ABC104B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let s = String.trim input
  pure ((if solve' s then "AC" else "WA") <> "\n")

solve' :: String -> Boolean
solve' s
  | (CodeUnits.charAt 0 s) /= Maybe.Just 'A' = false
  | let
      c = Unsafe.unsafePartial (Maybe.fromJust (CodeUnits.charAt 1 s))
    in
      c < 'a' || 'z' < c = false
  | let
      c =
        Unsafe.unsafePartial
          (Maybe.fromJust (CodeUnits.charAt ((CodeUnits.length s) - 1) s))
    in
      c < 'a' || 'z' < c = false
  | otherwise =
    let
      s' = CodeUnits.drop 2 s
      cs = CodeUnits.toCharArray s'
    in
      case Array.findIndex (eq 'C') cs of
        Maybe.Nothing -> false
        Maybe.Just index ->
          Array.all
            (\c -> 'a' <= c && c <= 'z')
            (Array.mapWithIndex (\i c -> if i == index then 'c' else c) cs)
