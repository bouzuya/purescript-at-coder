-- abc098_b
module ABC098B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Set as Set
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  Tuple.Tuple n s <-
    Either.note "n s"
      case splitByNL input of
        [nLine, s] -> do
          n <- Int.fromString nLine
          Maybe.Just (Tuple.Tuple n s)
        _ -> Maybe.Nothing
  pure ((show (solve' n s)) <> "\n")

solve' :: Int -> String -> Int
solve' n s = MonadRec.tailRec go (Tuple.Tuple 1 0)
  where
    go (Tuple.Tuple i max')
      | i == ((CodeUnits.length s) - 1) = MonadRec.Done max'
      | otherwise =
        let
          { before, after } = CodeUnits.splitAt i s
          beforeSet = Set.fromFoldable (CodeUnits.toCharArray before)
          afterSet = Set.fromFoldable (CodeUnits.toCharArray after)
        in
        MonadRec.Loop
          (Tuple.Tuple
            (i + 1)
            (max max' (Set.size (Set.intersection beforeSet afterSet))))
