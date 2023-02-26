module GameData where

import System.Random


-- direction Left, Right, Up and Down
data Direction = L | R | U | D
  deriving (Eq, Show)

-- Grid and relevant types
type Position = (Int, Int)

data Grid = Grid {  
  value :: Int,
  position :: Position,
  animationState :: AnimationState,
  scl :: Float, -- scale of grid
  progress :: Float, -- progress between grids; 0 is no progress, 1 is full progress
  prevValue :: Int, 
  prevPosition :: Position
}
  deriving (Eq, Show)

-- Board
type Board = [[Grid]]

-- Game State
data GameState = GameState {
  board :: Board,
  seed :: StdGen
}
  deriving (Show)

-- Game Result
data GameResult = EndOfGame GameState
                | ContinueGame GameState
  deriving (Show)

-- Bounce state of a grid used for GUI
data AnimationState =  Move | Merge | Grow | Shrink | End deriving (Eq, Show)