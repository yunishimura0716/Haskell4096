module TestConstant where

import GameData
import Data.List
-- a massive list of test constants

{- format: "board"_{boardSize}_{index}} -}

data Corner = UpperLeft | UpperRight | LowerLeft | LowerRight
  deriving (Show, Eq, Enum, Bounded)

{- BOARD SIZE 2 ; each board is manually constructed -}
-- empty board
board_2_0 :: Board
board_2_0 =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the upper left (unmoved)
board_2_UL :: Board
board_2_UL =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 2 (0, 1) End 1 1 2 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the upper right (unmoved)
board_2_UR :: Board
board_2_UR =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 2 (1, 1) End 1 0 2 (1, 1)]]

-- one 2 in the lower left (unmoved)
board_2_LL :: Board
board_2_LL =
  [[Grid 2 (0, 0) End 1 1 2 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the lower right (unmoved)
board_2_LR :: Board
board_2_LR =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 2 (1, 0) End 1 1 2 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the upper left (moved)
board_2_UL_m :: Board
board_2_UL_m =
  [[Grid 0 (0, 0) End  1 1 0 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 2 (0, 1) Move 1 0 2 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the upper right (moved)
board_2_UR_m :: Board
board_2_UR_m =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 0 (1, 0) End  1 1 0 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 2 (1, 1) Move 1 0 2 (1, 1)]]

-- one 2 in the lower left (moved)
board_2_LL_m :: Board
board_2_LL_m =
  [[Grid 2 (0, 0) Move 1 0 2 (0, 0), Grid 0 (1, 0) End 1 1 0 (1, 0)],
   [Grid 0 (0, 1) End  1 1 0 (0, 1), Grid 0 (1, 1) End 1 1 0 (1, 1)]]

-- one 2 in the lower right (moved)
board_2_LR_m :: Board
board_2_LR_m =
  [[Grid 0 (0, 0) End 1 1 0 (0, 0), Grid 2 (1, 0) Move 1 0 2 (1, 0)],
   [Grid 0 (0, 1) End 1 1 0 (0, 1), Grid 0 (1, 1) End  1 1 0 (1, 1)]]


{- BOARD SIZE 3 ; each board is manually constructed -}


{- BOARD SIZE 4 ; each board is manually constructed -}


{- BOARD SIZE 5 ; each board is manually constructed -}


{- BOARD SIZE ARBITRARY -}
boardNCorner :: Corner -> Int -> Int -> AnimationState -> Board
boardNCorner corner n val aState
  | corner == UpperLeft = 
    (Grid val (0, 0) aState 1 p val (0, 0):[ Grid 0 (x, 0) End 1 1 0 (x, 0) | x <- [1..n-1]]) -- first row
    : [[Grid 0 (x, y) End 1 1 0 (x, y) | x <- [0..n-1]] | y <- [1..n-1]] -- lower rows    
  | corner == UpperRight = 
    ([ Grid 0 (x, 0) End 1 1 0 (x, 0) | x <- [0..n-2]]++[Grid val (n-1, 0) aState 1 p val (n-1, 0)]) -- first row
    : [[Grid 0 (x, y) End 1 1 0 (x, y) | x <- [0..n-1]] | y <- [1..n-1]] -- lower rows
  | corner == LowerLeft = 
    [[Grid 0 (x, y) End 1 1 0 (x, y) | x <- [0..n-1]] | y <- [0..n-2]] -- upper rows
    ++ [Grid val (0, n-1) aState 1 p val (0, n-1) : [ Grid 0 (x, n-1) End 1 1 0 (x, n-1) | x <- [1..n-1]]]    
  | corner == LowerRight =
    [[Grid 0 (x, y) End 1 1 0 (x, y) | x <- [0..n-1]] | y <- [0..n-2]] -- upper rows
    ++ [[ Grid 0 (x, n-1) End 1 1 0 (x, n-1) | x <- [0..n-2]] -- first ..n-2 columns
      ++[Grid val (n-1, n-1) aState 1 p val (n-1, n-1)]]
  | otherwise = error "invalid corner value"
    where p = if aState == Move then 0 else 1




-- boardNCorner LowerLeft n a =
--   transpose (map reverse (boardNCorner UpperLeft n a))
-- boardNCorner LowerRight n a =
--   map reverse (transpose (map reverse (boardNCorner UpperLeft n a)))







-- data Grid = Grid {
--   value :: Int,
--   position :: Position,
--   animationState :: AnimationState,
--   scl :: Float, -- scale of grid
--   progress :: Float, -- progress between grids; 0 is no progress, 1 is full progress
--   prevValue :: Int,
--   prevPosition :: Position
-- }