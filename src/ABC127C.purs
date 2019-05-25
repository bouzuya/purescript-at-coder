-- abc127_c
module ABC127C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
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

int2 :: String -> Maybe (Tuple Int Int)
int2 s =
  case Array.mapMaybe Int.fromString (splitBySP s) of
    [a, b] -> Maybe.Just (Tuple.Tuple a b)
    _ -> Maybe.Nothing

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { head: nmLine, tail: lrLines } <-
    Either.note "lines" (Array.uncons (splitByNL input))
  Tuple.Tuple n m <- Either.note "n m" (int2 nmLine)
  let lrs = Array.mapMaybe int2 lrLines
  pure ((show (solve' n m lrs)) <> "\n")

solve' :: Int -> Int -> Array (Tuple Int Int) -> Int
solve' _ _ lrs = MonadRec.tailRec go { i: 0, min': bottom, max': top }
  where
    go { i, min', max' } =
      case Array.index lrs i of
        Maybe.Nothing ->
          MonadRec.Done (if max' >= min' then (max' - min' + 1) else 0)
        Maybe.Just (Tuple.Tuple l r) ->
          MonadRec.Loop
            { i: i + 1
            , min': max min' l
            , max': min max' r
            }


