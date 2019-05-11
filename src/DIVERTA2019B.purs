-- diverta2019_b
module DIVERTA2019B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { r, g, b, n } <-
    Either.note "r g b n"
      case Array.mapMaybe Int.fromString (splitBySP (String.trim input)) of
        [r, g, b, n] -> Maybe.Just { r, g, b, n }
        _ -> Maybe.Nothing
  pure ((show (solve' r g b n)) <> "\n")

solve' :: Int -> Int -> Int -> Int -> Int
solve' r g b n = ST.run do
  countRef <- STRef.new 0
  ST.for 0 ((min 3000 (n / r)) + 1) \r' -> do
    let r'' = r * r'
    ST.for 0 ((max 0 (min 3000 (((n - r'') / g)))) + 1) \g' -> do
      let g'' = g * g'
      let n'' = n - (r'' + g'')
      if n'' >= 0 && ((n'' `mod` b) == 0)
        then void (STRef.modify (add 1) countRef)
        else pure unit
  STRef.read countRef

