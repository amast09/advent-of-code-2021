module Day2.Dive (commandFromString, calculatePosition, calculateFinalPosition, Command (..), Direction (..), Position (..)) where

import Debug.Trace
import Text.Read (readMaybe)
import Text.Regex.TDFA

data Direction = Forward | Down | Up deriving (Eq, Show)

type Distance = Int

data Position = Position {horizontalPosition :: Int, depth :: Int} deriving (Eq, Show)

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
calculatePosition (Position {horizontalPosition = chp, depth = cd}) (Command Forward distance) =
  Position {horizontalPosition = chp + distance, depth = cd}
calculatePosition (Position {horizontalPosition = chp, depth = cd}) (Command Down distance) =
  Position {horizontalPosition = chp, depth = cd + distance}
calculatePosition (Position {horizontalPosition = chp, depth = cd}) (Command Up distance) =
  Position {horizontalPosition = chp, depth = cd - distance}

calculateFinalPosition :: [Command] -> Position
calculateFinalPosition commands = foldl calculatePosition Position {horizontalPosition = 0, depth = 0} commands
