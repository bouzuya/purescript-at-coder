module Test.Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable as Traversable
import Effect (Effect)
import Effect.Class as Class
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Test.Unit (Test)
import Test.Unit as TestUnit
import Test.Unit.Main as TestUnitMain

foreign import getSolve :: String -> String -> String

type Task = String
type Sample = { input :: String, output :: String, number :: Int }

readSample :: FilePath -> Task -> Int -> Effect Sample
readSample dir task i = do
  let inputFile = Path.concat [dir, task, (show i) <> "-input.txt"]
  let outputFile = Path.concat [dir, task, (show i) <> "-output.txt"]
  input <- FS.readTextFile Encoding.UTF8 inputFile
  output <- FS.readTextFile Encoding.UTF8 outputFile
  pure { input, output, number: i }

readSamples :: FilePath -> Task -> Effect (Array Sample)
readSamples dir task = do
  let taskDir = Path.concat [dir, task]
  fileCount <- map Array.length (FS.readdir taskDir)
  Traversable.traverse
    (readSample dir task)
    (Array.range 1 (fileCount / 2))

assertEquals :: forall a. Eq a => Show a => String -> a -> a -> Test
assertEquals message expected actual
  | actual == expected = TestUnit.success
  | otherwise =
    TestUnit.failure
      (String.joinWith
        " "
        [ message
        , "expected " <> (show expected) <> ", got " <> (show actual)
        ])

testTask :: Sample -> Task -> Test
testTask { input, output, number } task =
  assertEquals ("Sample " <> (show number) <> ":") output (getSolve task input)

main :: Effect Unit
main = do
  taskMaybe <- Class.liftEffect (Process.lookupEnv "TASK") -- e.g. "ABC111B"
  case taskMaybe of
    Maybe.Nothing -> pure unit
    Maybe.Just task -> do
      TestUnitMain.runTest do
      TestUnit.test (task <> " samples") do
        samples <- Class.liftEffect (readSamples "test" task)
        Foldable.for_ samples \sample -> do
          testTask sample task
