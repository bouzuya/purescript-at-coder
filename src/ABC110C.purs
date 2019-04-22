-- abc110_c
module ABC110C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Char as Char
import Data.Either as Either
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  s <- Either.note "s" (Array.head lines)
  t <- Either.note "t" (Array.last lines)
  pure ((if solve' s t then "Yes" else "No") <> "\n")

solve' :: String -> String -> Boolean
solve' s t = ST.run do
  let l = CodeUnits.length s
  cs1 <- STArray.thaw (Array.replicate 26 0)
  cs2 <- STArray.thaw (Array.replicate 26 0)
  equalRef <- STRef.new true
  ST.for 0 l \i -> do
    let
      (Tuple.Tuple c1 c2) =
        Maybe.maybe' (\_ -> Unsafe.unsafeCrashWith "invalid") identity do
          c1 <- CodeUnits.charAt i s
          c2 <- CodeUnits.charAt i t
          pure (Tuple.Tuple c1 c2)
      i1 = (Char.toCharCode c1) - (Char.toCharCode 'a')
      i2 = (Char.toCharCode c2) - (Char.toCharCode 'a')
    x1Maybe <- STArray.peek i1 cs1
    x2Maybe <- STArray.peek i2 cs2
    case x1Maybe, x2Maybe of
      (Maybe.Just 0), (Maybe.Just 0) -> do
        a1 <- STArray.unsafeFreeze cs1
        let n = (Array.length (Array.filter (notEq zero) a1)) + 1
        void (STArray.poke i1 n cs1)
        void (STArray.poke i2 n cs2)
        pure unit
      (Maybe.Just x1), (Maybe.Just x2) ->
        if x1 == x2
          then pure unit
          else void (STRef.write false equalRef)
      _, _ ->
        Unsafe.unsafeCrashWith "invalid"
  STRef.read equalRef
