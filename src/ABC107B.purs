-- abc107_B
module ABC107B
  ( main
  , solve
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either as Either
import Data.Int as Int
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

splitBySP :: String -> Array String
splitBySP s = String.split (String.Pattern " ") s

solve :: String -> String
solve input = Either.either (\s -> Unsafe.unsafeCrashWith s) identity do
  { head: hwLine, tail: as } <-
    Either.note "uncons" (Array.uncons (splitByNL input))
  Tuple.Tuple h w <-
    case Array.mapMaybe Int.fromString (splitBySP hwLine) of
      [h, w] -> Either.Right (Tuple.Tuple h w)
      _ -> Either.Left "h w"
  let blacks = map (\s -> map (eq '#') (CodeUnits.toCharArray s)) as
  pure ((solve' h w blacks) <> "\n")

solve' :: Int -> Int -> Array (Array Boolean) -> String
solve' h w blacks = format (updateW (updateH blacks))
  where
    format xs =
      String.joinWith
        "\n"
        (map
          (\x -> String.joinWith "" (map (\b -> if b then "#" else ".") x))
          xs)
    updateH xs = Array.filter (Array.any identity) xs
    updateW xs =
      let
        w' =
          case Array.head xs of
            Maybe.Nothing -> Unsafe.unsafeCrashWith "invalid"
            Maybe.Just line -> Array.length line
      in
        ST.run do
          sta <- STArray.unsafeThaw (Array.replicate w' false)
          ST.foreach xs \line -> do
            ST.for 0 w' \i -> do
              if Unsafe.unsafePartial (Array.unsafeIndex line i)
                then void (STArray.poke i true sta)
                else pure unit
          a <- STArray.unsafeFreeze sta
          pure
            (map
              (\x -> map Tuple.fst (Array.filter Tuple.snd (Array.zip x a)))
              xs)
