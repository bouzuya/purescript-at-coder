-- abc104_c
module ABC104C
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Int.Bits as Bits
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
  { head: dgLine, tail: pcLines } <-
    Either.note "lines" (Array.uncons (splitByNL input))
  Tuple.Tuple d g <- Either.note "d g" (int2 dgLine)
  let pcs = Array.mapMaybe int2 pcLines
  pure ((show (solve' d g pcs)) <> "\n")

solve' :: Int -> Int -> Array (Tuple Int Int) -> Int
solve' d g pcs = ST.run do
  resultRef <- STRef.new top
  ST.for 0 (Int.pow 2 d) \i -> do
    csumRef <- STRef.new 0
    psumRef <- STRef.new 0
    ST.for 0 d \j -> do
      when (notEq 0 (Bits.and i (Bits.shl 1 j))) do
        let Tuple.Tuple p c = Unsafe.unsafePartial (Array.unsafeIndex pcs j)
        void (STRef.modify (add ((j + 1) * 100 * p + c)) csumRef)
        void (STRef.modify (add p) psumRef)
    csum <- STRef.read csumRef
    psum <- STRef.read psumRef
    iresult <-
      if csum >= g
        then pure psum
        else do
          csumRef' <- STRef.new 0
          psumRef' <- STRef.new 0
          ST.for 0 d \j' -> do
            let j = d - j' - 1
            when (eq 0 (Bits.and i (Bits.shl 1 j))) do
              let
                Tuple.Tuple p c =
                  Unsafe.unsafePartial (Array.unsafeIndex pcs j)
              csum' <- STRef.read csumRef'
              when ((csum + csum') < g) do
                if (csum + csum' + ((j + 1) * 100 * p)) >= g
                  then do
                    ST.for 0 p \_ -> do
                      csum'' <- STRef.read csumRef'
                      when ((csum + csum'') < g) do
                        void (STRef.modify (add ((j + 1) * 100)) csumRef')
                        void (STRef.modify (add 1) psumRef')
                  else do
                    void (STRef.modify (add (((j + 1) * 100 * p) + c)) csumRef')
                    void (STRef.modify (add p) psumRef')
          psum' <- STRef.read psumRef'
          pure (psum + psum')
    STRef.modify (min iresult) resultRef
  STRef.read resultRef
