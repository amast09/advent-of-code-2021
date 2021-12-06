module Day3.BinaryDiagnosticSpec (spec) where

import Day3.BinaryDiagnostic
import Test.Hspec

spec :: Spec
spec = do
  describe "calculatePowerConsumption" $ do
    it "calculates the right power consumption based on the diagnostics" $ do
      let binaryDiagnostics =
            [ "00100",
              "11110",
              "10110",
              "10111",
              "10101",
              "01111",
              "00111",
              "11100",
              "10000",
              "11001",
              "00010",
              "01010"
            ]
      calculatePowerConsumption binaryDiagnostics `shouldBe` 198

    it "handles varying length diagnostics" $ do
      let binaryDiagnostics =
            [ "0010",
              "1111",
              "1011",
              "1011",
              "1010",
              "0111",
              "0011",
              "1110",
              "1000",
              "1100",
              "0001",
              "0101"
            ]
      calculatePowerConsumption binaryDiagnostics `shouldBe` 44
