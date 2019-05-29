-- abc102_c
module ABC102C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
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
  pure ((BigInt.toString (solve' n as)) <> "\n")

solve' :: Int -> Array Int -> BigInt
solve' n as = ST.run do
  sta <- STArray.thaw as
  ST.for 0 n \i -> do
    STArray.modify i (_ - (i + 1)) sta
  _ <- STArray.sort sta
  as' <- STArray.unsafeFreeze sta
  let b = median n as'
  pure (Array.foldl (\s a' -> s + BigInt.fromInt (Ord.abs (a' - b))) zero as')

median :: Int -> Array Int -> Int
median n sorted
  | Int.odd n = Unsafe.unsafePartial (Array.unsafeIndex sorted (n / 2))
  | otherwise =
      ((Unsafe.unsafePartial (Array.unsafeIndex sorted (n / 2 - 1)))
      + (Unsafe.unsafePartial (Array.unsafeIndex sorted (n / 2)))) / 2
