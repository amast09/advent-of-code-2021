module Day2.PositionWithAim (PositionWithAim (..)) where

data PositionWithAim = PositionWithAim {horizontalPosition :: Int, depth :: Int, aim :: Int} deriving (Eq, Show)
