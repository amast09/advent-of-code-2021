module LibSpec (spec) where

import Lib (fileToInts)
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
