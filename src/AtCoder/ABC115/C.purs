module AtCoder.ABC115.C
  ( solve
  ) where

import Prelude

import Control.Monad.Rec.Class as MonadRec
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  { n, k } <-
    Either.note "n k" do
      line <- Array.head lines
      case
        Array.mapMaybe
          Int.fromString
          (String.split (String.Pattern " ") line) of
        [n, k] -> Maybe.Just { n, k }
        _ -> Maybe.Nothing
  let hs = Array.mapMaybe Int.fromString (Array.drop 1 lines)
  pure (show (solve'' n k hs))

solve'' :: Int -> Int -> Array Int -> Int
solve'' n k hs =
  let
    sorted = Array.sort hs
    go { i, r }
      | i == (n - k + 1) = MonadRec.Done r
      | otherwise =
        let
          minH = Unsafe.unsafePartial (Array.unsafeIndex sorted i)
          maxH = Unsafe.unsafePartial (Array.unsafeIndex sorted (i + k - 1))
          newR = maxH - minH
        in
          MonadRec.Loop { i: i + 1, r: min r newR }
  in
    MonadRec.tailRec go { i: 0, r: top }
