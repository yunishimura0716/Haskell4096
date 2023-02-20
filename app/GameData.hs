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
  bouncestate :: BounceState,
  scl :: Float
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
data BounceState = Grow | Shrink | End deriving (Eq, Show)