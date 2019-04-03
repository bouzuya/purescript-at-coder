module AtCoder.ABC119.C
  ( solve
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
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
  nabcLine <- Either.note "n a b c line" (Array.head lines)
  { n, a, b, c } <-
    case
      Array.mapMaybe
        Int.fromString
        (String.split (String.Pattern " ") nabcLine) of
      [n, a, b, c] -> Right { n, a, b, c }
      _ -> Left "n a b c"
  let ls = Array.mapMaybe Int.fromString (Array.drop 1 lines)
  if Array.length ls == n then pure unit else Left "l lines"
  pure (show (solve'' a b c ls))

solve'' :: Int -> Int -> Int -> Array Int -> Int
solve'' a b c ls =
  case go 0 0 0 0 of
    Nothing -> Unsafe.unsafeCrashWith "go"
    Just n -> n
  where
    go :: Int -> Int -> Int -> Int -> Maybe Int
    go i a' b' c' =
      case Array.index ls i of
        Nothing ->
          if a' == 0 || b' == 0 || c' == 0
            then Nothing
            else Just
              ( (Ord.abs (a' - a))
              + (Ord.abs (b' - b))
              + (Ord.abs (c' - c))
              - 30
              )
        Just l ->
          Array.foldl
            (\m1 m2 -> min <$> (m1 <|> m2) <*> (m2 <|> m1)) -- FIXME
            Nothing
            [ (go (i + 1) a' b' c')
            , map (add 10) (go (i + 1) (a' + l) b' c')
            , map (add 10) (go (i + 1) a' (b' + l) c')
            , map (add 10) (go (i + 1) a' b' (c' + l))
            ]
