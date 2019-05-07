-- abc102_b
module ABC102B
  ( main
  , solve
  ) where

import Prelude

import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.Traversable as Traversable
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
  Tuple.Tuple n as <-
    Either.note "n as"
      case splitByNL input of
        [nLine, asLine] -> do
          n <- Int.fromString nLine
          as <- Traversable.traverse Int.fromString (splitBySP asLine)
          pure (Tuple.Tuple n as)
        _ -> Maybe.Nothing
  pure ((show (solve' n as)) <> "\n")

solve' :: Int -> Array Int -> Int
solve' n as = Maybe.fromMaybe' (\_ -> Unsafe.unsafeCrashWith "") do
  min' <- Traversable.minimum as
  max' <- Traversable.maximum as
  pure (Ord.abs (max' - min'))
