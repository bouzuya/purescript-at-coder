-- abc125_c
module ABC125C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
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
  asLine <- Either.note "vs" (Array.last lines)
  let as = Array.mapMaybe Int.fromString (splitBySP asLine)
  pure ((show (solve' n as)) <> "\n")

solve' :: Int -> Array Int -> Int
solve' n as = ST.run do
  sta <- STArray.empty
  maxRef <- STRef.new Maybe.Nothing
  ST.for 0 n \i -> do
    xRef <-
      STRef.new
        (Unsafe.unsafePartial
          (Maybe.fromJust
            (Array.index as (if i == n - 1 then 0 else n - 1))))
    max' <- STRef.read maxRef
    ST.for 0 n \j -> do
      x <- STRef.read xRef
      if (x == 1) || (Maybe.maybe false (\max'' -> x < max'') max')
        then pure unit
        else do
          if j == i
            then pure unit
            else do
              let y = Unsafe.unsafePartial (Maybe.fromJust (Array.index as j))
              void (STRef.write (gcd x y) xRef)
    x <- STRef.read xRef
    void (STRef.write (max max' (Maybe.Just x)) maxRef)
  max' <- STRef.read maxRef
  pure (Unsafe.unsafePartial (Maybe.fromJust max'))
