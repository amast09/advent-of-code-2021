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
      calculatePowerConsumption binaryDiagnostics `shouldBe` Right 198

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
      calculatePowerConsumption binaryDiagnostics `shouldBe` Right 44

    it "handles bad input" $ do
      let binaryDiagnostics =
            [ "0010",
              "1111",
              "1011",
              "1011",
              "1010",
              "gibberish",
              "0011",
              "1110",
              "1000",
              "1100",
              "0001",
              "0101"
            ]
      calculatePowerConsumption binaryDiagnostics `shouldBe` Left "Invalid input"

  describe "calculateRating" $ do
    describe "calculateOxygenRating" $ do
      let calculateOxygenRating = calculateRating O2RatingType

      it "calculates the right oxygen rating based on the diagnostics" $ do
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
        calculateOxygenRating binaryDiagnostics `shouldBe` Right 23

      it "handles bad input" $ do
        let binaryDiagnostics =
              [ "0010",
                "1111",
                "1011",
                "1011",
                "1010",
                "gibberish",
                "0011",
                "1110",
                "1000",
                "1100",
                "0001",
                "0101"
              ]
        calculateOxygenRating binaryDiagnostics `shouldBe` Left "Unable to match diagnostic lines to ratings"

    describe "calculateC02ScrubberRating" $ do
      let calculateC02ScrubberRating = calculateRating CO2RatingType

      it "calculates the right C02 scrubber rating based on the diagnostics" $ do
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
        calculateC02ScrubberRating binaryDiagnostics `shouldBe` Right 10

      it "handles bad input" $ do
        let binaryDiagnostics =
              [ "0010",
                "1111",
                "1011",
                "1011",
                "1010",
                "gibberish",
                "0011",
                "1110",
                "1000",
                "1100",
                "0001",
                "0101"
              ]
        calculateC02ScrubberRating binaryDiagnostics `shouldBe` Left "Unable to match diagnostic lines to ratings"

  describe "calculateLifeSupportRating" $ do
    it "calculates the right life support rating based on the diagnostics" $ do
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
      calculateLifeSupportRating binaryDiagnostics `shouldBe` Right 230

    it "handles bad input" $ do
      let binaryDiagnostics =
            [ "0010",
              "1111",
              "1011",
              "1011",
              "1010",
              "gibberish",
              "0011",
              "1110",
              "1000",
              "1100",
              "0001",
              "0101"
            ]
      calculateLifeSupportRating binaryDiagnostics `shouldBe` Left "Unable to match diagnostic lines to ratings"
