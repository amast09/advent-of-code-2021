module Day2.DiveSpec (spec) where

import Day2.Dive
import Test.Hspec

spec :: Spec
spec = do
  describe "commandFromString" $ do
    it "parses a Forward command" $ do
      commandFromString "forward 1" `shouldBe` (Just (Command Forward 1))

    it "parses a Down command" $ do
      commandFromString "down 2" `shouldBe` (Just (Command Down 2))

    it "parses an Up command" $ do
      commandFromString "up 3" `shouldBe` (Just (Command Up 3))

    it "parses an invalid command" $ do
      commandFromString "3 up" `shouldBe` Nothing
      commandFromString "up" `shouldBe` Nothing
      commandFromString "gibberish" `shouldBe` Nothing

  describe "calculatePosition" $ do
    it "allows moving forward" $ do
      let command = Command Forward 7
          currentPosition = Position {horizontalPosition = 3, depth = 4}
          expectedFinalPosition = Position {horizontalPosition = 10, depth = 4}
      calculatePosition currentPosition command `shouldBe` expectedFinalPosition

    it "allows moving up" $ do
      let command = Command Up 3
          currentPosition = Position {horizontalPosition = 3, depth = 4}
          expectedFinalPosition = Position {horizontalPosition = 3, depth = 1}
      calculatePosition currentPosition command `shouldBe` expectedFinalPosition

    it "allows moving down" $ do
      let command = Command Down 37
          currentPosition = Position {horizontalPosition = 3, depth = 4}
          expectedFinalPosition = Position {horizontalPosition = 3, depth = 41}
      calculatePosition currentPosition command `shouldBe` expectedFinalPosition

    describe "calculateFinalPosition" $ do
      it "determines a final position given a list of commands" $ do
        let commands =
              [ Command Forward 5,
                Command Down 5,
                Command Forward 8,
                Command Up 3,
                Command Down 8,
                Command Forward 2
              ]
            expectedFinalPosition = Position {horizontalPosition = 15, depth = 10}
        calculateFinalPosition commands `shouldBe` expectedFinalPosition
