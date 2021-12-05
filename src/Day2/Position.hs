module Day2.Position (Position (..)) where

data Position = Position {horizontalPosition :: Int, depth :: Int} deriving (Eq, Show)
