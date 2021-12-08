module Day1.SonarSweeper (getNumberOfDepthIncreases, getNumberOfWindowedDepthIncreases) where

data DepthAcc = DepthAcc {numberOfIncreases :: Int, lastDepth :: Maybe Int}

data WindowAcc = WindowAcc
  { numWinIncreases :: Int,
    lastWinDepth :: Maybe Int,
    restOfMeasurements :: [Int]
  }

getNumberOfDepthIncreases :: [Int] -> Int
getNumberOfDepthIncreases depths =
  numberOfIncreases $ foldl nextDepthAcc DepthAcc {numberOfIncreases = 0, lastDepth = Nothing} depths

nextDepthAcc :: DepthAcc -> Int -> DepthAcc
nextDepthAcc (DepthAcc {lastDepth = Nothing}) currentDepth = DepthAcc {numberOfIncreases = 0, lastDepth = Just currentDepth}
nextDepthAcc (DepthAcc {numberOfIncreases = incs, lastDepth = Just (ld)}) currentDepth
  | ld < currentDepth = DepthAcc {numberOfIncreases = incs + 1, lastDepth = Just currentDepth}
  | otherwise = DepthAcc {numberOfIncreases = incs, lastDepth = Just currentDepth}

nextWindowedAcc :: WindowAcc -> WindowAcc
nextWindowedAcc (WindowAcc {numWinIncreases = nwi, lastWinDepth = Nothing, restOfMeasurements = (m1 : m2 : m3 : xs)}) =
  nextWindowedAcc
    WindowAcc
      { numWinIncreases = nwi,
        lastWinDepth = Just (m1 + m2 + m3),
        restOfMeasurements = ([m2] ++ [m3] ++ xs)
      }
nextWindowedAcc (WindowAcc {numWinIncreases = nwi, lastWinDepth = Just lwd, restOfMeasurements = (m1 : m2 : m3 : xs)}) =
  let nextDepth = m1 + m2 + m3
      nextDepthIncreases = if nextDepth > lwd then (nwi + 1) else nwi
   in nextWindowedAcc
        WindowAcc
          { numWinIncreases = nextDepthIncreases,
            lastWinDepth = Just nextDepth,
            restOfMeasurements = ([m2] ++ [m3] ++ xs)
          }
nextWindowedAcc a = a

getNumberOfWindowedDepthIncreases :: [Int] -> Int
getNumberOfWindowedDepthIncreases depths =
  let initialWinAcc =
        WindowAcc
          { numWinIncreases = 0,
            lastWinDepth = Nothing,
            restOfMeasurements = depths
          }
      finalWinAcc = nextWindowedAcc initialWinAcc
      result = numWinIncreases finalWinAcc
   in result
