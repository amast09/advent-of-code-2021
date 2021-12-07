module Day3.BinaryDiagnostic (calculatePowerConsumption, calculateOxygenRating, calculateC02ScrubberRating, calculateLifeSupportRating) where

import Control.Lens
import Data.Char (digitToInt)
import Data.List (foldl')
import Util

type Rate = Int

type DiagnosticLine = String

data Acc = Acc {currentIdx :: Int, rates :: [Rate]} deriving (Show, Eq)

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

calculatePowerConsumption :: [DiagnosticLine] -> Either String Int
calculatePowerConsumption diagnosticLines =
  let rates = foldl accDiagnosticLine (Right Acc {currentIdx = 0, rates = []}) diagnosticLines
   in fmap getPowerConsumption rates

data BinaryPosition = Zero | One deriving (Show, Eq)

checkBitInLine :: BinaryPosition -> Int -> DiagnosticLine -> Bool
checkBitInLine bp bitIndex diagnosticLine = case diagnosticLine ^? element bitIndex of
  (Just bit) -> (bp == One && bit == '1') || (bp == Zero && bit == '0')
  Nothing -> False

getRateForBit :: Int -> DiagnosticLine -> Either String Rate
getRateForBit bitIndex diagnosticLine = case diagnosticLine ^? element bitIndex of
  (Just bit) -> bitToRate bit
  Nothing -> Left "Bad Diagnostic Line"

data DiagnosticLinesWithIdx = DiagnosticLinesWithIdx Int [DiagnosticLine] deriving (Show, Eq)

toNextDls :: DiagnosticLinesWithIdx -> Rate -> DiagnosticLinesWithIdx
toNextDls (DiagnosticLinesWithIdx idx dls) rate =
  let binaryPositionToFilter = if rate >= 0 then One else Zero
      diagnosticFilter = checkBitInLine binaryPositionToFilter idx
      nextDls = filter diagnosticFilter dls
   in DiagnosticLinesWithIdx (idx + 1) nextDls

get02Rating :: DiagnosticLinesWithIdx -> DiagnosticLinesWithIdx
get02Rating (DiagnosticLinesWithIdx idx ([])) = DiagnosticLinesWithIdx idx []
get02Rating (DiagnosticLinesWithIdx idx (dl : [])) = DiagnosticLinesWithIdx idx [dl]
get02Rating (DiagnosticLinesWithIdx idx dls) =
  let ratesResult = foldl (\acc dl -> (+) <$> acc <*> getRateForBit idx dl) (Right 0) dls
   in case ratesResult of
        (Right rate) -> get02Rating $ toNextDls (DiagnosticLinesWithIdx idx dls) rate
        (Left _) -> (DiagnosticLinesWithIdx idx [])

parse02RatingResult :: DiagnosticLinesWithIdx -> Either String Int
parse02RatingResult (DiagnosticLinesWithIdx _ (dl : [])) = Right $ toDec dl
parse02RatingResult _ = Left "Unable to match diagnostic lines to ratings"

calculateOxygenRating :: [DiagnosticLine] -> Either String Int
calculateOxygenRating diagnosticLines =
  let o2RatingResult = get02Rating (DiagnosticLinesWithIdx 0 diagnosticLines)
      o2Rating = parse02RatingResult o2RatingResult
   in o2Rating

----------------------------------------------------------------------------------------

toNextDls2 :: DiagnosticLinesWithIdx -> Rate -> DiagnosticLinesWithIdx
toNextDls2 (DiagnosticLinesWithIdx idx dls) rate =
  let binaryPositionToFilter = if rate >= 0 then Zero else One
      diagnosticFilter = checkBitInLine binaryPositionToFilter idx
      nextDls = filter diagnosticFilter dls
   in DiagnosticLinesWithIdx (idx + 1) nextDls

get02Rating2 :: DiagnosticLinesWithIdx -> DiagnosticLinesWithIdx
get02Rating2 (DiagnosticLinesWithIdx idx ([])) = DiagnosticLinesWithIdx idx []
get02Rating2 (DiagnosticLinesWithIdx idx (dl : [])) = DiagnosticLinesWithIdx idx [dl]
get02Rating2 (DiagnosticLinesWithIdx idx dls) =
  let ratesResult = foldl (\acc dl -> (+) <$> acc <*> getRateForBit idx dl) (Right 0) dls
   in case ratesResult of
        (Right rate) -> get02Rating2 $ toNextDls2 (DiagnosticLinesWithIdx idx dls) rate
        (Left _) -> (DiagnosticLinesWithIdx idx [])

calculateC02ScrubberRating :: [DiagnosticLine] -> Either String Int
calculateC02ScrubberRating diagnosticLines =
  let o2RatingResult = get02Rating2 (DiagnosticLinesWithIdx 0 diagnosticLines)
      o2Rating = parse02RatingResult o2RatingResult
   in o2Rating

calculateLifeSupportRating :: [DiagnosticLine] -> Either String Int
calculateLifeSupportRating diagnosticLines =
  let c02Rating = calculateC02ScrubberRating diagnosticLines
      o2Rating = calculateOxygenRating diagnosticLines
   in (*) <$> c02Rating <*> o2Rating
