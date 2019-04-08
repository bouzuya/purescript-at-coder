module AtCoder.ABC114.C
  ( solve
  ) where

import Prelude

import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Math as Math
import Partial.Unsafe as Unsafe

solve :: String -> String
solve input =
  case solve' input of
    Either.Left s -> Unsafe.unsafeCrashWith s
    Either.Right s -> s

solve' :: String -> Either String String
solve' input = do
  n <- Either.note "n" (Int.fromString (String.trim input))
  let m = Int.ceil ((Math.log (Int.toNumber n)) * Math.log10e)
  pure (show (solve'' n m ""))

ok :: Int -> String -> Boolean
ok m s = Maybe.isJust do
  _ <- String.indexOf (String.Pattern "3") s
  _ <- String.indexOf (String.Pattern "5") s
  _ <- String.indexOf (String.Pattern "7") s
  x <- Int.fromString s
  if x <= m then Maybe.Just unit else Maybe.Nothing

solve'' :: Int -> Int -> String -> Int
solve'' m 0 s = if ok m s then 1 else 0
solve'' m n s =
  let
    c0 = if ok m s then 1 else 0
    c1 = solve'' m (n - 1) (s <> "3")
    c2 = solve'' m (n - 1) (s <> "5")
    c3 = solve'' m (n - 1) (s <> "7")
  in
    c0 + c1 + c2 + c3
