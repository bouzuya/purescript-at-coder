-- abc109_b
module ABC109B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
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
  let lines = splitByNL input
  { head: nLine, tail: ws } <- Either.note "uncons" (Array.uncons lines)
  n <- Either.note "n" (Int.fromString nLine)
  pure ((toYesNo (solve' n ws)) <> "\n")

solve' :: Int -> Array String -> Boolean
solve' n ws = MonadRec.tailRec go (Tuple.Tuple n Maybe.Nothing)
  where
    go (Tuple.Tuple i c) =
      case Array.index ws (n - i) of
        Maybe.Nothing -> MonadRec.Done true
        Maybe.Just w ->
          let
            h = CodeUnits.charAt 0 w
            l = CodeUnits.charAt ((CodeUnits.length w) - 1) w
          in
            if
              (Maybe.isNothing c) ||
              ( (c == h) &&
                (Maybe.isNothing
                  (Array.find (eq w) (Array.take (n - i) ws))))
              then MonadRec.Loop (Tuple.Tuple (i - 1) l)
              else MonadRec.Done false
