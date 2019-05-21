-- abc096_b
module ABC096B
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
  { a, b, c, k } <-
    case splitByNL input of
      [abcLine, kLine] -> Either.note "int" do
        { a, b, c } <-
          case Array.mapMaybe Int.fromString (splitBySP abcLine) of
            [a, b, c] -> Maybe.Just { a, b, c }
            _ -> Maybe.Nothing
        k <- Int.fromString kLine
        pure { a, b, c, k }
      _ -> Either.Left "lines"
  pure ((show (solve' a b c k)) <> "\n")

solve' :: Int -> Int -> Int -> Int -> Int
solve' a b c k =
  let max' = max a (max b c)
  in a + b + c + (max' * ((Int.pow 2 k) - 1))
