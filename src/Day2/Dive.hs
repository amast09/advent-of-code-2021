module Day2.Dive (commandFromString, calculatePosition, calculateFinalPosition, calculateFinalPositionWithAim, calculatePositionWithAim, Command (..), Direction (..)) where

import Day2.Position as P
import Day2.PositionWithAim as PWA
import Debug.Trace
import Text.Read (readMaybe)
import Text.Regex.TDFA

data Direction = Forward | Down | Up deriving (Eq, Show)

type Distance = Int

data Command = Command Direction Distance deriving (Eq, Show)

traceShow' arg = traceShow arg arg

directionFromString :: String -> Maybe Direction
directionFromString "up" = Just Up
directionFromString "down" = Just Down
directionFromString "forward" = Just Forward
directionFromString _ = Nothing

commandFromString :: String -> Maybe Command
commandFromString commandString =
  let regex = "^(up|down|forward) ([0-9]+)$"
      (_, _, _, matches) = commandString =~ regex :: (String, String, String, [String])
      result = case matches of
        (direction : distance : _) -> fmap Command (directionFromString direction) <*> (readMaybe distance :: Maybe Int)
        _ -> Nothing
   in result

calculatePosition :: Position -> Command -> Position
calculatePosition (Position {P.horizontalPosition = chp, P.depth = cd}) (Command Forward distance) =
  Position {P.horizontalPosition = chp + distance, P.depth = cd}
calculatePosition (Position {P.horizontalPosition = chp, P.depth = cd}) (Command Down distance) =
  Position {P.horizontalPosition = chp, P.depth = cd + distance}
calculatePosition (Position {P.horizontalPosition = chp, P.depth = cd}) (Command Up distance) =
  Position {P.horizontalPosition = chp, P.depth = cd - distance}

calculatePositionWithAim :: PositionWithAim -> Command -> PositionWithAim
calculatePositionWithAim (PositionWithAim {PWA.horizontalPosition = chp, PWA.depth = cd, aim = a}) (Command Forward distance) =
  PositionWithAim {PWA.horizontalPosition = chp + distance, PWA.depth = cd + (a * distance), PWA.aim = a}
calculatePositionWithAim (PositionWithAim {PWA.horizontalPosition = chp, PWA.depth = cd, PWA.aim = a}) (Command Down distance) =
  PositionWithAim {PWA.horizontalPosition = chp, PWA.depth = cd, aim = (a + distance)}
calculatePositionWithAim (PositionWithAim {PWA.horizontalPosition = chp, PWA.depth = cd, PWA.aim = a}) (Command Up distance) =
  PositionWithAim {PWA.horizontalPosition = chp, PWA.depth = cd, aim = (a - distance)}

calculateFinalPosition :: [Command] -> Position
calculateFinalPosition commands = foldl calculatePosition Position {P.horizontalPosition = 0, P.depth = 0} commands

calculateFinalPositionWithAim :: [Command] -> PositionWithAim
calculateFinalPositionWithAim commands = foldl calculatePositionWithAim PositionWithAim {PWA.horizontalPosition = 0, PWA.depth = 0, PWA.aim = 0} commands
