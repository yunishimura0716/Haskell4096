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

-- Board
type Board = [[Grid]]

-- Game State
data GameState = GameState {
  board :: Board,
  randomSeed :: StdGen
}

-- Game Result
data GameResult = EndOfGame 
                | ContinueGame GameState