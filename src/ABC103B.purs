-- abc103_b
module ABC103B
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.String as String
import Data.String.CodeUnits as CodeUnits
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

toYesNo :: Boolean -> String
toYesNo yes
  | yes = "Yes"
  | otherwise = "No"

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  Tuple.Tuple s t <-
    case splitByNL input of
      [s, t] -> Either.Right (Tuple.Tuple s t)
      _ -> Either.Left "s t"
  pure ((toYesNo (solve' s t)) <> "\n")

solve' :: String -> String -> Boolean
solve' s t = go 0
  where
    l = CodeUnits.length s
    t' = Maybe.Just t
    go i
      | i == l = false
      | otherwise =
        let s' = (<>) <$> (CodeUnits.slice i l s) <*> (CodeUnits.slice 0 i s)
        in if s' == t' then true else go (i + 1)
