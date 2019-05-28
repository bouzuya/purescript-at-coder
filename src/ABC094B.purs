-- abc094_b
module ABC094B
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
  { n, m, x, as } <-
    Either.note "invalid"
      case splitByNL input of
        [nmxLine, asLine] -> do
          { n, m, x } <-
            case Array.mapMaybe Int.fromString (splitBySP nmxLine) of
              [n, m, x] -> Maybe.Just { n, m, x }
              _ -> Maybe.Nothing
          let as = Array.mapMaybe Int.fromString (splitBySP asLine)
          pure { n, m, x, as }
        _ -> Maybe.Nothing
  pure ((show (solve' n m x as)) <> "\n")

solve' :: Int -> Int -> Int -> Array Int -> Int
solve' n m x as =
  let
    l = Array.length (Array.filter (_ < x) as)
    g = m - l
  in
    min l g
