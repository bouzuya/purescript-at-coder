-- abc091_b
module ABC091B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Ref as Ref
import Foreign.Object as Object
import Foreign.Object.ST as STObject
import Foreign.Object.ST.Unsafe as STObjectUnsafe
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
  n <- Either.note "n" ((Array.head lines) >>= Int.fromString)
  let ss = Array.drop 1 (Array.take (n + 1) lines)
  let lines' = Array.drop (n + 1) lines
  m <- Either.note "m" ((Array.head lines') >>= Int.fromString)
  let ts = Array.drop 1 lines'
  pure ((show (solve' n ss m ts)) <> "\n")

solve' :: Int -> Array String -> Int -> Array String -> Int
solve' n ss m ts = ST.run do
  sto <- STObject.new
  ST.for 0 n \i -> do
    let s = Unsafe.unsafePartial (Array.unsafeIndex ss i)
    maybe <- STObject.peek s sto
    _ <- STObject.poke s (Maybe.maybe 1 (_ + 1) maybe) sto
    pure unit
  ST.for 0 m \i -> do
    let t = Unsafe.unsafePartial (Array.unsafeIndex ts i)
    maybe <- STObject.peek t sto
    _ <- STObject.poke t (Maybe.maybe 0 (_ - 1) maybe) sto
    pure unit
  o <- STObjectUnsafe.unsafeFreeze sto
  pure
    (max
      0
      (Maybe.fromMaybe'
        (\_ -> Unsafe.unsafeCrashWith "")
        (Array.last (Array.sort (Object.values o)))))

