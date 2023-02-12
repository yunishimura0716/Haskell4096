module Board where

import Data.List
import GameData
import Grid


-- create a board with a number of rows/columns
createBoard :: Int -> [[Grid]]
createBoard n =
  [ [grid | grid <- grids, getY grid == i] | i <- [0..(n-1)] ]
    where
      grids = map (\x -> Grid 0 (mod x n, div x n)) [0..lastIndex]
      lastIndex = n * n - 1
      getY = snd . position

-- rotate the board by direction to be left shifting 
toLeftShiftBoard :: Direction -> Board -> Board
toLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = transpose board
  | dirct == D = map reverse (transpose board)

-- rotate the board by direction to be orignal from left shifting 
fromLeftShiftBoard :: Direction -> Board -> Board
fromLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = transpose board
  | dirct == D = transpose (map reverse board) 

-- remove blank grid from left
removeBlanks :: [Grid] -> [Grid]
removeBlanks [] = []
removeBlanks (hd:tr) =
  if (value hd) == 0
    then removeBlanks tr 
  else
    hd:(removeBlanks tr)

-- add blanks to the tail of the list
addBlanks :: Int -> Int -> Int -> [Grid] -> [Grid]
addBlanks 0 _ _ grids = grids
addBlanks n _ y [] = [(Grid 0 (i, y)) | i <- [0..(n-1)]]
addBlanks n x y grids =
  addBlanks (n-1) x y (grids ++ [Grid 0 (x+1, y)])

-- left shifting on the list without blanks
leftShift :: [Grid] -> [Grid]
leftShift [] = []
leftShift [grid] = [grid]
leftShift (hd:hd2:tr) 
  | result = grid:(leftShift tr)
  | not result = hd:(leftShift (hd2:tr))
    where 
      (grid, result) = combineGrids hd hd2
      

-- behave left shifting on the list of grids
leftShiftGrids :: [Grid] -> [Grid]
leftShiftGrids [grid] = [grid] 
leftShiftGrids grids =
  addBlanks n x y resultGrids
    where
      n = (length grids) - (length resultGrids)
      (x, y) = position (last grids)
      resultGrids = leftShift gridsNoBlank
      gridsNoBlank = removeBlanks grids
      
-- behave left shifting on the board
leftShiftBoard :: Board -> Board
leftShiftBoard board = map leftShiftGrids board

-- behave shifting with direction on the board
shiftBoard :: Direction -> Board -> Board
shiftBoard dirct board = fromLeftShiftBoard dirct (leftShiftBoard (toLeftShiftBoard dirct board))
