module Day2.DiveSpec (spec) where

import Day2.Dive
import Day2.Position as P
import Day2.PositionWithAim as PWA
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
          currentPosition = Position {P.horizontalPosition = 3, P.depth = 4}
          expectedFinalPosition = Position {P.horizontalPosition = 10, P.depth = 4}
      calculatePosition currentPosition command `shouldBe` expectedFinalPosition

    it "allows moving up" $ do
      let command = Command Up 3
          currentPosition = Position {P.horizontalPosition = 3, P.depth = 4}
          expectedFinalPosition = Position {P.horizontalPosition = 3, P.depth = 1}
      calculatePosition currentPosition command `shouldBe` expectedFinalPosition

    it "allows moving down" $ do
      let command = Command Down 37
          currentPosition = Position {P.horizontalPosition = 3, P.depth = 4}
          expectedFinalPosition = Position {P.horizontalPosition = 3, P.depth = 41}
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
            expectedFinalPosition = Position {P.horizontalPosition = 15, P.depth = 10}
        calculateFinalPosition commands `shouldBe` expectedFinalPosition

    describe "calculatePositionWithAim" $ do
      it "allows moving forward" $ do
        let command = Command Forward 7
            currentPositionWithAim = PositionWithAim {PWA.horizontalPosition = 3, PWA.depth = 4, aim = 3}
            expectedFinalPosition = PositionWithAim {PWA.horizontalPosition = 10, PWA.depth = 25, aim = 3}
        calculatePositionWithAim currentPositionWithAim command `shouldBe` expectedFinalPosition

      it "allows moving up" $ do
        let command = Command Up 3
            currentPositionWithAim = PositionWithAim {PWA.horizontalPosition = 93, PWA.depth = 33, aim = 9}
            expectedFinalPosition = PositionWithAim {PWA.horizontalPosition = 93, PWA.depth = 33, aim = 6}
        calculatePositionWithAim currentPositionWithAim command `shouldBe` expectedFinalPosition

      it "allows moving down" $ do
        let command = Command Down 9
            currentPositionWithAim = PositionWithAim {PWA.horizontalPosition = 32, PWA.depth = 774, aim = 94}
            expectedFinalPosition = PositionWithAim {PWA.horizontalPosition = 32, PWA.depth = 774, aim = 103}
        calculatePositionWithAim currentPositionWithAim command `shouldBe` expectedFinalPosition

    describe "calculateFinalPositionWithAim" $ do
      it "determines a final position given a list of commands" $ do
        let commands =
              [ Command Forward 5,
                Command Down 5,
                Command Forward 8,
                Command Up 3,
                Command Down 8,
                Command Forward 2
              ]
            expectedFinalPositionWithAim = PositionWithAim {PWA.horizontalPosition = 15, PWA.depth = 60, aim = 10}
        calculateFinalPositionWithAim commands `shouldBe` expectedFinalPositionWithAim
