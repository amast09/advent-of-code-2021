module Lib (calculateAnswers, fileToInts) where

import Control.Exception.Base (IOException, catch)
import Day1.SonarSweeper (getNumberOfDepthIncreases)
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

calculateAnswers :: IO ()
calculateAnswers = do
  (Just day1Puzzle1Data) <- fileToInts "src/Day1/sonar-sweeper-data.txt"
  let day1Puzzle1Result = getNumberOfDepthIncreases day1Puzzle1Data
  putStrLn ("The Answer to Day 1 Puzzle 1 is: " ++ (show day1Puzzle1Result))
