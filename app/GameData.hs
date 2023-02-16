module GameData where

import System.Random


-- direction Left, Right, Up and Down
data Direction = L | R | U | D
  deriving (Eq, Show)

-- Grid and relevant types
type Position = (Int, Int)

data Grid = Grid {
  value :: Int,
  position :: Position
}
  deriving (Show)

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