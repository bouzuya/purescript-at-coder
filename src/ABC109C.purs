-- abc109_c
module ABC109C
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.NonEmpty as NonEmpty
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
  nxLine <- Either.note "n x line" (Array.head lines)
  xsLine <- Either.note "xs line" (Array.last lines)
  Tuple.Tuple n x <-
    case Array.mapMaybe Int.fromString (splitBySP nxLine) of
      [n, x] -> Either.Right (Tuple.Tuple n x)
      _ -> Either.Left "n x"
  let xs = Array.mapMaybe Int.fromString (splitBySP xsLine)
  pure ((show (solve' n x xs)) <> "\n")

solve' :: Int -> Int -> Array Int -> Int
solve' n x xs =
  let
    xs' = map (\x' -> Ord.abs (x' - x)) xs
    xs'' =
      NonEmptyArray.toNonEmpty
        (Unsafe.unsafePartial (Maybe.fromJust (NonEmptyArray.fromArray xs')))
  in
    NonEmpty.foldl1 gcd xs''
