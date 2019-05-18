module AtCoder.ABC119.B
  ( solve
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.String as String
import Partial.Unsafe as Unsafe

data C
  = BTC Number
  | JPY Int

fromString :: String -> String -> Maybe C
fromString xs "BTC" = Number.fromString xs >>= (pure <<< BTC)
fromString xs "JPY" = Int.fromString xs >>= (pure <<< JPY)
fromString _ _ = Nothing

toNumber :: C -> Number
toNumber (BTC n) = n * 380000.0
toNumber (JPY i) = Int.toNumber i

solve :: String -> String
solve input =
  case solve' input of
    Left s -> Unsafe.unsafeCrashWith s
    Right s -> s

solve' :: String -> Either String String
solve' input = do
  let lines = String.split (String.Pattern "\n") (String.trim input)
  n <- Either.note "n" do
    ns <- Array.head lines
    Int.fromString ns
  let xuLines = Array.drop 1 lines
  Maybe.maybe
    (Left "num")
    (Right <<< show)
    (Array.foldRecM
      (\a xuLine -> do
        c <-
          case String.split (String.Pattern " ") xuLine of
            [xs, us] -> fromString xs us
            _ -> Nothing
        pure (a + (toNumber c)))
      zero
      xuLines)

