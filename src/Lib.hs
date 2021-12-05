module Lib (calculateAnswers, fileToInts, fileToCommands) where

import Control.Exception.Base (IOException, catch)
import Day1.SonarSweeper (getNumberOfDepthIncreases, getNumberOfWindowedDepthIncreases)
import Day2.Dive (Command, calculateFinalPosition, calculateFinalPositionWithAim, commandFromString)
import Day2.Position as P
import Day2.PositionWithAim as PWA
import Text.Read (readMaybe)

safeLoadFile' :: String -> IO (Maybe String)
safeLoadFile' p =
  (Just <$> readFile p)
    `catch` ((\e -> pure Nothing) :: IOException -> IO (Maybe String))

stringsToIntsAcc :: Maybe [Int] -> String -> Maybe [Int]
stringsToIntsAcc (Nothing) _ = Nothing
stringsToIntsAcc (Just ints) s = case (readMaybe s :: Maybe Int) of
  (Just i) -> Just (ints ++ [i])
  (Nothing) -> Nothing

fileToInts :: String -> IO (Maybe [Int])
fileToInts filePath = do
  maybeFileContents <- safeLoadFile' filePath
  let maybeLines = fmap lines maybeFileContents
  let maybeStringsToMaybeInts = (foldl stringsToIntsAcc (Just []))
  return (maybeLines >>= maybeStringsToMaybeInts)

-- At some point the 2 acc functions could be made into a generic function
stringsToCommandsAcc :: Maybe [Command] -> String -> Maybe [Command]
stringsToCommandsAcc (Nothing) _ = Nothing
stringsToCommandsAcc (Just commands) s = case (commandFromString s) of
  (Just c) -> Just (commands ++ [c])
  (Nothing) -> Nothing

fileToCommands :: String -> IO (Maybe [Command])
fileToCommands filePath = do
  maybeFileContents <- safeLoadFile' filePath
  let maybeLines = fmap lines maybeFileContents
  let maybeStringsToMaybeCommands = (foldl stringsToCommandsAcc (Just []))
  return (maybeLines >>= maybeStringsToMaybeCommands)

calculateAnswers :: IO ()
calculateAnswers = do
  (Just day1PuzzleData) <- fileToInts "src/Day1/sonar-sweeper-data.txt"
  let day1Puzzle1Result = getNumberOfDepthIncreases day1PuzzleData
  let day1Puzzle2Result = getNumberOfWindowedDepthIncreases day1PuzzleData
  putStrLn ("The Answer to Day 1 Puzzle 1 is: " ++ (show day1Puzzle1Result))
  putStrLn ("The Answer to Day 1 Puzzle 2 is: " ++ (show day1Puzzle2Result))

  (Just day2PuzzleData) <- fileToCommands "src/Day2/dive-data.txt"
  let puzzle1FinalPosition = calculateFinalPosition day2PuzzleData
  let day2Puzzle1Result = (P.horizontalPosition puzzle1FinalPosition) * (P.depth puzzle1FinalPosition)
  let puzzle2FinalPosition = calculateFinalPositionWithAim day2PuzzleData
  let day2Puzzle2Result = (PWA.horizontalPosition (puzzle2FinalPosition)) * (PWA.depth puzzle2FinalPosition)
  putStrLn ("The Answer to Day 2 Puzzle 1 is: " ++ (show day2Puzzle1Result))
  putStrLn ("The Answer to Day 2 Puzzle 2 is: " ++ (show day2Puzzle2Result))
