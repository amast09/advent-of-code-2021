module Day1.SonarSweeper (getNumberOfDepthIncreases) where

data Acc = Acc {numberOfIncreases :: Int, lastDepth :: Maybe Int}

getNumberOfDepthIncreases :: [Int] -> Int
getNumberOfDepthIncreases depths = numberOfIncreases $ foldl getNextAcc Acc {numberOfIncreases = 0, lastDepth = Nothing} depths

getNextAcc :: Acc -> Int -> Acc
getNextAcc (Acc {lastDepth = Nothing}) currentDepth = Acc {numberOfIncreases = 0, lastDepth = Just currentDepth}
getNextAcc (Acc {numberOfIncreases = incs, lastDepth = Just (ld)}) currentDepth
  | ld < currentDepth = Acc {numberOfIncreases = incs + 1, lastDepth = Just currentDepth}
  | otherwise = Acc {numberOfIncreases = incs, lastDepth = Just currentDepth}
