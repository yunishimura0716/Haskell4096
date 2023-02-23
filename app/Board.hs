{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import Data.List
import GameData
import Grid
import Graphics.Gloss
import Render
import UIConstant


boardSize :: Int -> Float
boardSize n = gridSize * factor + dividerSize * (factor+1)
  where
    factor = fromIntegral n

boardFrame :: Int -> Picture
boardFrame numSlots = pictures [fullboard, slots]
  where
    fullboard = color boardBackground $ rectangleSolid (boardSize numSlots) (boardSize numSlots)
    slots = translate fitGridToBoard fitGridToBoard . pictures $ map slotRender [0,1..lastIndex]
      where
        fitGridToBoard
          | numSlots == 4 = fitGridToBoard4
          | numSlots == 5 = fitGridToBoard5
        lastIndex = numSlots * numSlots - 1
    slotRender n = translate (x*size) (y*size) $ color slotBackground (rectangleSolid gridSize gridSize)
      where
        size = gridSize + dividerSize
        x = fromIntegral $ mod n numSlots
        y = fromIntegral $ div n numSlots


instance Model Board where
  render b = pictures [(boardFrame n), grids]
    where
      grids = translate fitGridToBoard fitGridToBoard (pictures $ map render flatBoard)
      fitGridToBoard
        | n == 4 = fitGridToBoard4
        | n == 5 = fitGridToBoard5
      flatBoard = flattenBoard b
      n = length b

-- flatten the board to list of grids
flattenBoard :: Board -> [Grid]
flattenBoard board =
  foldl (\acc x -> acc ++ x) [] board

-- create a board with a number of rows/columns
createBoard :: Int -> [[Grid]]
createBoard n =
  [ [grid | grid <- grids, getY grid == i] | i <- [0..(n-1)] ]
    where
      grids = map (\x -> Grid 0 (mod x n, div x n) End 1) [0..lastIndex]
      lastIndex = n * n - 1
      getY = snd . position

-- clear the positions on the board
clearBoard :: Board -> Board
clearBoard board =
  [ [(Grid (getGridValue x y) (x,y) (getGridBS x y) (getGridScale x y)) | x <- [0..(n-1)]] | y <- [0..(n-1)] ]
    where
      getGridValue x y = value (getGrid x y)
      getGridScale x y = scl (getGrid x y)
      getGridBS x y    = bouncestate (getGrid x y)
      getGrid x y = (board !! y) !! x
      n = length board

-- rotate the board by direction to be left shifting 
toLeftShiftBoard :: Direction -> Board -> Board
toLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = map reverse (transpose board)
  | dirct == D = transpose board 

-- rotate the board by direction to be orignal from left shifting 
fromLeftShiftBoard :: Direction -> Board -> Board
fromLeftShiftBoard dirct board
  | dirct == L = board
  | dirct == R = map reverse board
  | dirct == U = transpose (map reverse board) 
  | dirct == D = transpose board


-- remove blank grid from left
removeBlanks :: Int -> Int -> [Grid] -> [Grid]
removeBlanks _ _ [] = []
removeBlanks x y (hd:tr) =
  if (value hd) == 0
    then removeBlanks x y tr 
  else
    (Grid (value hd) (x, y) End 1):(removeBlanks (x+1) y tr)

-- add blanks to the tail of the list
addBlanks :: Int -> Int -> Int -> [Grid] -> [Grid]
addBlanks 0 _ _ grids = grids
addBlanks n _ y [] = [(Grid 0 (i, y) End 1) | i <- [0..(n-1)]]
addBlanks n x y grids =
  addBlanks (n-1) (x+1) y (grids ++ [Grid 0 (x, y) End 1])

-- left shifting on the list without blanks
leftShift :: Int -> Int -> [Grid] -> [Grid]
leftShift _ _ [] = []
leftShift x y [grid] = [Grid (value grid) (x, y) End 1]
leftShift x y (hd:hd2:tr) 
  | result = grid:(leftShift (x+1) y tr)
  | not result = hd:(leftShift (x+1) y (hd2:tr))
    where 
      (grid, result) = combineGrids x y hd hd2
      

-- behave left shifting on the list of grids
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
      
-- behave left shifting on the board
leftShiftBoard :: Board -> Board
leftShiftBoard board = map leftShiftGrids board

-- behave shifting with direction on the board
shiftBoard :: Direction -> Board -> Board
shiftBoard dirct board = 
  clearBoard newBoard
  where
    newBoard = fromLeftShiftBoard dirct (leftShiftBoard (toLeftShiftBoard dirct board))

-- check if boards are equal
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