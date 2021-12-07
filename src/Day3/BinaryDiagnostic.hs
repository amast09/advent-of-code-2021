module Day3.BinaryDiagnostic (calculatePowerConsumption) where

import Control.Lens
import Data.Char (digitToInt)
import Data.List (foldl')

data Acc = Acc {currentIdx :: Int, rates :: [Int]} deriving (Show, Eq)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

bitToRate :: Char -> Either String Int
bitToRate '0' = Right (-1)
bitToRate '1' = Right 1
bitToRate _ = Left "Invalid input"

accRate :: Acc -> Int -> Acc
accRate Acc {currentIdx = ci, rates = rs} nextRate =
  let nextRates = case rs ^? element ci of
        Just currentRate -> rs & element ci .~ (currentRate + nextRate)
        Nothing -> rs ++ [nextRate]
   in Acc {currentIdx = ci + 1, rates = nextRates}

accBit :: Either String Acc -> Char -> Either String Acc
accBit acc bit = fmap accRate acc <*> bitToRate bit

accDiagnosticLine :: Either String Acc -> String -> Either String Acc
accDiagnosticLine (Right Acc {rates = rs}) diagnosticLine =
  foldl accBit (Right Acc {currentIdx = 0, rates = rs}) diagnosticLine
accDiagnosticLine acc _ = acc

gammaValueToBinary :: Int -> Char
gammaValueToBinary v = if v > 0 then '1' else '0'

epsilonValueToBinary :: Int -> Char
epsilonValueToBinary v = if v < 0 then '1' else '0'

getPowerConsumption :: Acc -> Int
getPowerConsumption Acc {rates = rs} =
  let gammaRate = toDec $ map gammaValueToBinary rs
      epsilonRate = toDec $ map epsilonValueToBinary rs
   in (gammaRate * epsilonRate)

calculatePowerConsumption :: [String] -> Either String Int
calculatePowerConsumption diagnosticLines =
  let gammaMeasurements = foldl accDiagnosticLine (Right Acc {currentIdx = 0, rates = []}) diagnosticLines
   in fmap getPowerConsumption gammaMeasurements
