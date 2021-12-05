module LibSpec (spec) where

import Day2.Dive
import Lib (fileToCommands, fileToInts)
import Test.Hspec

spec :: Spec
spec = do
  describe "fileToInts" $ do
    it "returns Just [Ints] when the file contains `Int`'s separated by new lines" $ do
      let expectedInts = Just [1, 2, 34, 5]
      actualInts <- fileToInts "test/data/integers.txt"
      actualInts `shouldBe` expectedInts

    it "returns Nothing when given a file that does not exist" $ do
      actualInts <- fileToInts "DOES NOT EXIST"
      actualInts `shouldBe` Nothing

    it "returns Nothing when given a file that doesn't have Ints" $ do
      actualInts <- fileToInts "test/data/floats.txt"
      actualInts `shouldBe` Nothing

  describe "fileToCommands" $ do
    it "returns Just [Commands] when the file contains `Command`'s separated by new lines" $ do
      let expectedCommands =
            Just
              [ Command Forward 5,
                Command Down 5,
                Command Forward 8,
                Command Up 3,
                Command Down 8,
                Command Forward 2
              ]
      actualCommands <- fileToCommands "test/data/dive.txt"
      actualCommands `shouldBe` expectedCommands

    it "returns Nothing when given a file that does not exist" $ do
      actualCommands <- fileToCommands "DOES NOT EXIST"
      actualCommands `shouldBe` Nothing

    it "returns Nothing when given a file that doesn't have Ints" $ do
      actualCommands <- fileToCommands "test/data/floats.txt"
      actualCommands `shouldBe` Nothing
