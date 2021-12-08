module Day3.BinaryDiagnostic (calculatePowerConsumption, calculateRating, calculateLifeSupportRating, RatingType (..)) where

import Control.Lens
import Data.Char (digitToInt)
import Data.List (foldl')
import Util

type Rate = Int

type DiagnosticLine = String

data Acc = Acc {currentIdx :: Int, rates :: [Rate]} deriving (Show, Eq)

data BinaryPosition = Zero | One deriving (Show, Eq)

data DiagnosticLinesWithIdx = DiagnosticLinesWithIdx Int [DiagnosticLine] deriving (Show, Eq)

data RatingType = O2RatingType | CO2RatingType deriving (Show, Eq)

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

checkBitInLine :: BinaryPosition -> Int -> DiagnosticLine -> Bool
checkBitInLine bp bitIndex diagnosticLine = case diagnosticLine ^? element bitIndex of
  (Just bit) -> (bp == One && bit == '1') || (bp == Zero && bit == '0')
  Nothing -> False

getRateForBit :: Int -> DiagnosticLine -> Either String Rate
getRateForBit bitIndex diagnosticLine = case diagnosticLine ^? element bitIndex of
  (Just bit) -> bitToRate bit
  Nothing -> Left "Bad Diagnostic Line"

toNextDls :: RatingType -> DiagnosticLinesWithIdx -> Rate -> DiagnosticLinesWithIdx
toNextDls rt (DiagnosticLinesWithIdx idx dls) rate =
  let binaryPositionToFilter =
        if rate >= 0
          then (if rt == O2RatingType then One else Zero)
          else (if rt == CO2RatingType then One else Zero)
      diagnosticFilter = checkBitInLine binaryPositionToFilter idx
      nextDls = filter diagnosticFilter dls
   in DiagnosticLinesWithIdx (idx + 1) nextDls

getRating :: RatingType -> DiagnosticLinesWithIdx -> DiagnosticLinesWithIdx
getRating _ (DiagnosticLinesWithIdx idx ([])) = DiagnosticLinesWithIdx idx []
getRating _ (DiagnosticLinesWithIdx idx (dl : [])) = DiagnosticLinesWithIdx idx [dl]
getRating rt (DiagnosticLinesWithIdx idx dls) =
  let ratesResult = foldl (\acc dl -> (+) <$> acc <*> getRateForBit idx dl) (Right 0) dls
   in case ratesResult of
        (Right rate) -> (getRating rt) $ (toNextDls rt) (DiagnosticLinesWithIdx idx dls) rate
        (Left _) -> (DiagnosticLinesWithIdx idx [])

parseRatingResult :: DiagnosticLinesWithIdx -> Either String Int
parseRatingResult (DiagnosticLinesWithIdx _ (dl : [])) = Right $ toDec dl
parseRatingResult _ = Left "Unable to match diagnostic lines to ratings"

calculateRating :: RatingType -> [DiagnosticLine] -> Either String Int
calculateRating rt diagnosticLines =
  let o2RatingResult = getRating rt (DiagnosticLinesWithIdx 0 diagnosticLines)
      o2Rating = parseRatingResult o2RatingResult
   in o2Rating

calculateLifeSupportRating :: [DiagnosticLine] -> Either String Int
calculateLifeSupportRating diagnosticLines =
  let c02Rating = calculateRating CO2RatingType diagnosticLines
      o2Rating = calculateRating O2RatingType diagnosticLines
   in (*) <$> c02Rating <*> o2Rating
