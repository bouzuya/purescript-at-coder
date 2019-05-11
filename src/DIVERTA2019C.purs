-- diverta2019_c
module DIVERTA2019C
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
  { head: nLine, tail: ss } <-
    Either.note "lines" (Array.uncons (splitByNL input))
  n <- Either.note "n" (Int.fromString nLine)
  pure ((show (solve' n ss)) <> "\n")

solve' :: Int -> Array String -> Int
solve' n ss = MonadRec.tailRec go { i: 0, c: 0, ac: 0, bc: 0, abc: 0 }
  where
    go { i, c, ac, bc, abc } =
      case Array.index ss i of
        Maybe.Nothing ->
          MonadRec.Done
            (c + (min ac bc) + (max 0 (abc - 1)) +
              (if abc > 0 && (ac > (min ac bc)) then 1 else 0) +
              (if abc > 0 && (bc > (min ac bc)) then 1 else 0))
        Maybe.Just s ->
          let
            sc = (Array.length (String.split (String.Pattern "AB") s)) - 1
            sa = (CodeUnits.takeRight 1 s) == "A"
            sb = (CodeUnits.take 1 s) == "B"
            sab = sa && sb
          in
            MonadRec.Loop
              { i: i + 1
              , c: c + sc
              , ac: ac + if not sab && sa then 1 else 0
              , bc: bc + if not sab && sb then 1 else 0
              , abc: abc + if sab then 1 else 0
              }
