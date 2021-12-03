module Day1.SonarSweeper (getNumberOfDepthIncreases, getNumberOfWindowedDepthIncreases) where

data Acc = Acc {numberOfIncreases :: Int, lastDepth :: Maybe Int}

getNumberOfDepthIncreases :: [Int] -> Int
getNumberOfDepthIncreases depths = numberOfIncreases $ foldl getNextAcc Acc {numberOfIncreases = 0, lastDepth = Nothing} depths

getNextAcc :: Acc -> Int -> Acc
getNextAcc (Acc {lastDepth = Nothing}) currentDepth = Acc {numberOfIncreases = 0, lastDepth = Just currentDepth}
getNextAcc (Acc {numberOfIncreases = incs, lastDepth = Just (ld)}) currentDepth
  | ld < currentDepth = Acc {numberOfIncreases = incs + 1, lastDepth = Just currentDepth}
  | otherwise = Acc {numberOfIncreases = incs, lastDepth = Just currentDepth}

data WinAcc = WinAcc {numWinIncreases :: Int, lastWinDepth :: Maybe Int, restOfMeasurements :: [Int]}

getWinIncrease :: WinAcc -> WinAcc
getWinIncrease (WinAcc {numWinIncreases = nwi, lastWinDepth = Nothing, restOfMeasurements = (m1 : m2 : m3 : xs)}) =
  getWinIncrease
    WinAcc
      { numWinIncreases = nwi,
        lastWinDepth = Just (m1 + m2 + m3),
        restOfMeasurements = ([m2] ++ [m3] ++ xs)
      }
getWinIncrease (WinAcc {numWinIncreases = nwi, lastWinDepth = Just lwd, restOfMeasurements = (m1 : m2 : m3 : xs)}) =
  let nextDepth = m1 + m2 + m3
      nextDepthIncreases = if nextDepth > lwd then (nwi + 1) else nwi
   in getWinIncrease
        WinAcc
          { numWinIncreases = nextDepthIncreases,
            lastWinDepth = Just nextDepth,
            restOfMeasurements = ([m2] ++ [m3] ++ xs)
          }
getWinIncrease a = a

getNumberOfWindowedDepthIncreases :: [Int] -> Int
getNumberOfWindowedDepthIncreases depths =
  numWinIncreases
    ( getWinIncrease
        WinAcc
          { numWinIncreases = 0,
            lastWinDepth = Nothing,
            restOfMeasurements = depths
          }
    )
