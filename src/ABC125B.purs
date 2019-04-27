-- abc125_b
module ABC125B
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
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  vsLine <- Either.note "vs" (Array.head (Array.drop 1 lines))
  csLine <- Either.note "cs" (Array.head (Array.drop 2 lines))
  let
    vs = Array.mapMaybe Int.fromString (splitBySP vsLine)
    cs = Array.mapMaybe Int.fromString (splitBySP csLine)
  pure ((show (solve' n vs cs)) <> "\n")

solve' :: Int -> Array Int -> Array Int -> Int
solve' n vs cs = MonadRec.tailRec go (Tuple.Tuple 0 0)
  where
    go (Tuple.Tuple i xy) =
      case
        do
          v <- Array.index vs i
          c <- Array.index cs i
          pure (Tuple.Tuple v c) of
        Maybe.Nothing -> MonadRec.Done xy
        Maybe.Just (Tuple.Tuple v c) ->
          if v > c
            then MonadRec.Loop (Tuple.Tuple (i + 1) (xy + v - c))
            else MonadRec.Loop (Tuple.Tuple (i + 1) xy)
