{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import Data.List
import GameData
import Grid
import Graphics.Gloss
import Render
import UIConstant


-- Compute the board size with a given integer, which is the number of rows
boardSize :: Int -> Float
boardSize n = gridSize * factor + dividerSize * (factor+1)
  where
    factor = fromIntegral n

-- Given number of rows, this retuns the Picture object for board
boardFrame :: Int -> Picture
boardFrame numSlots = pictures [fullboard, slots]
  where
    fullboard = color boardBackground $ rectangleSolid (boardSize numSlots) (boardSize numSlots)
    slots = translate fitGridToBoard fitGridToBoard . pictures $ map slotRender [0,1..lastIndex]
      where
        fitGridToBoard = fitGridToBoardN numSlots
        lastIndex = numSlots * numSlots - 1
    slotRender n = translate (x*size) (y*size) $ color slotBackground (rectangleSolid gridSize gridSize)
      where
        size = gridSize + dividerSize
        x = fromIntegral $ mod n numSlots
        y = fromIntegral $ div n numSlots


-- Render function for Board type. This will call a render funciton for each grids
instance Model Board where
  render b = pictures [(boardFrame n), grids]
    where
      grids = translate fitGridToBoard fitGridToBoard (pictures $ map render flatBoard)
      fitGridToBoard = fitGridToBoardN n
      flatBoard = flattenBoard b
      n = length b

-- Flatten the board to list of grids
flattenBoard :: Board -> [Grid]
flattenBoard board =
  foldl (\acc x -> acc ++ x) [] board

-- Create a board with a number of rows/columns
createBoard :: Int -> [[Grid]]
createBoard n =
  [ [grid | grid <- grids, getY grid == i] | i <- [0..(n-1)] ]
    where
      grids = map (\x -> Grid 0 (mod x n, div x n) End 1) [0..lastIndex]
      lastIndex = n * n - 1
      getY = snd . position

-- For the given board, it modifies the position values of each grids and
-- returns a board with correction position. 
clearBoard :: Board -> Board
clearBoard board =
  [ [(Grid (getGridValue x y) (x,y) (getGridBS x y) (getGridScale x y)) | x <- [0..(n-1)]] | y <- [0..(n-1)] ]
    where
      getGridValue x y = value (getGrid x y)
      getGridScale x y = scl (getGrid x y)
      getGridBS x y    = bouncestate (getGrid x y)
      getGrid x y = (board !! y) !! x
      n = length board

-- Rotate the board by direction to be left shifting 
-- Left shifting is the case that the grids will shift to the left in the board (direction = L)
toLeftShiftBoard :: Direction -> Board -> Board
toLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = map reverse (transpose board)
  | dirct == D = transpose board 

-- Rotate the board by direction to be orignal from left shifting 
-- `toLeftShiftBoard` function should be called with the given board before we call this
fromLeftShiftBoard :: Direction -> Board -> Board
fromLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = transpose (map reverse board) 
  | dirct == D = transpose board


-- Remove blank grid from the list of grids.
-- x and y are position values for next unblank grid
removeBlanks :: Int -> Int -> [Grid] -> [Grid]
removeBlanks _ _ [] = []
removeBlanks x y (hd:tr) =
  if (value hd) == 0
    then removeBlanks x y tr 
  else
    (Grid (value hd) (x, y) End 1):(removeBlanks (x+1) y tr)

-- Add blank grids to the tail of the list
-- n: the number of blank grides it's going to insert
-- x & y: position values for next blank grid
addBlanks :: Int -> Int -> Int -> [Grid] -> [Grid]
addBlanks 0 _ _ grids = grids
addBlanks n _ y [] = [(Grid 0 (i, y) End 1) | i <- [0..(n-1)]]
addBlanks n x y grids =
  addBlanks (n-1) (x+1) y (grids ++ [Grid 0 (x, y) End 1])

-- Execute left shifting on the list of grids, and returns the result
-- x & y: position values for next grid
leftShift :: Int -> Int -> [Grid] -> [Grid]
leftShift _ _ [] = []
leftShift x y [grid] = [Grid (value grid) (x, y) End 1]
leftShift x y (hd:hd2:tr) 
  | result = grid:(leftShift (x+1) y tr)
  | not result = hd:(leftShift (x+1) y (hd2:tr))
    where 
      (grid, result) = combineGrids x y hd hd2
      

-- Behave left shifting on the row of board
-- Delete the blank grids first, execute left shifting, and add blank grids to the end
leftShiftGrids :: [Grid] -> [Grid]
leftShiftGrids [grid] = [grid] 
leftShiftGrids grids =
  addBlanks n (x+1) y resultGrids
    where
      n = (length grids) - (length resultGrids)
      (x, _) = position (last resultGrids)
      resultGrids = leftShift 0 y gridsNoBlank
      gridsNoBlank = removeBlanks 0 y grids
      (_, y) = position (last grids)
      
-- Behave left shifting on the board
-- Apply `leftShiftGrids` function to the each row of board
leftShiftBoard :: Board -> Board
leftShiftBoard board = map leftShiftGrids board

-- Behave shifting with direction on the board
shiftBoard :: Direction -> Board -> Board
shiftBoard dirct board = 
  clearBoard newBoard
  where
    newBoard = fromLeftShiftBoard dirct (leftShiftBoard (toLeftShiftBoard dirct board))

-- Check if boards are equal
boardsEqual :: Board -> Board -> Bool
boardsEqual b1 b2
    | length b1 /= length b2 = False
    | length (flattenBoard b1) /= length (flattenBoard b2) = False
    | otherwise =
      not (any gridsUnequal (zip (concat b1) (concat b2)))
      where
        gridsUnequal (grid1, grid2) =
          (value grid1 /= value grid2) || 
          (position grid1 /= position grid2)