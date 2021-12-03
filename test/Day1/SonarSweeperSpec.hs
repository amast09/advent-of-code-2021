module Day1.SonarSweeperSpec (spec) where

import Day1.SonarSweeper (getNumberOfDepthIncreases)
import Test.Hspec

spec :: Spec
spec = do
  describe "getNumberOfDepthIncreases" $ do
    it "returns the number of depth increases" $ do
      let input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      getNumberOfDepthIncreases input `shouldBe` (7 :: Int)

    it "returns 0 for an empty array" $ do
      getNumberOfDepthIncreases [] `shouldBe` (0 :: Int)
