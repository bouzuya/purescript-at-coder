-- abc106_c
module ABC106C
  ( main
  , solve
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
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

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  let lines = splitByNL input
  s <- Either.note "s" (Array.head lines)
  k <- Either.note "k" ((Array.last lines) >>= BigInt.fromString )
  pure ((solve' s k) <> "\n")

solve' :: String -> BigInt -> String
solve' s k =
  let
    cs = CodeUnits.toCharArray s
    cs' =
      case Array.findIndex (notEq '1') cs of
        Maybe.Nothing -> ['1']
        Maybe.Just i -> Array.take (i + 1) cs
    last = Unsafe.unsafePartial (Maybe.fromJust (Array.last cs'))
  in
    CodeUnits.singleton
      if (BigInt.fromInt (Array.length cs')) <= k
        then last
        else
          let
            k' =
              Unsafe.unsafePartial
                (Maybe.fromJust (Int.fromString (BigInt.toString k)))
          in
            Maybe.fromMaybe last (Array.index cs k')
