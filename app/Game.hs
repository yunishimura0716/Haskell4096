module Game where

import System.Random
import Data.List
import GameData
import Board
import Grid
import UIConstant
import Render


-- Render function defined for Game type.
-- It will call a render function for Board type
instance Model GameState where
	render g = render $ board g

-- Util function to return random number from provided random seed and the list of nubmers
giveRandomElement :: StdGen -> [Int] -> Int
giveRandomElement generator giveList = giveList !! rand where
    n = length giveList
    (rand, _) = randomR (0,(n-1)) generator

-- Insert a random Grid to the board, return the row with inserted new grid
-- randV: the selected random number, used for new grid's value
-- chosenY: the y coordinate that will have new grid
-- i: represents the current row of board
insertFunc :: Int -> Int -> Int -> Board -> [Grid]
insertFunc randV chosenY i board =
  if i == chosenY
    then reverse ((Grid randV (x, chosenY) End 0 1 randV (x, chosenY)) : (tail (reverse grids)))
  else grids
    where
      x = length grids - 1
      grids = board !! i


-- Insert a grid with random number (2 or 4)
-- It takes the direction, random seed and current board, then return the board with inserted new grid
randomInsert :: Direction -> StdGen -> Board -> Board
randomInsert dirct seed board =
  clearBoard (fromLeftShiftBoard dirct newBoard)
    where
      newBoard = [(insertFunc randomValue chosenY i lfBoard) | i <- [0..(n-1)]]
      randomValue = giveRandomElement seed [2,4]
      chosenY = giveRandomElement seed possibleYs
      possibleYs = [i | i <- [0..(n-1)], (value (last (lfBoard !! i))) == 0]
      n = length board
      lfBoard = toLeftShiftBoard dirct board

-- Identify the given game state is able to continue taking an action
-- check if the board will change with every direction
isLost :: GameState -> Bool
isLost game =
  b == left_b && b == right_b && b == up_b && b == down_b
    where
      down_b = shiftBoard D b
      up_b = shiftBoard U b
      right_b = shiftBoard R b
      left_b = shiftBoard L b
      b = board game

-- define game type
type Game = Direction -> GameState -> GameResult

-- Play a game
-- It takes the direction and curretn game state, then returns the result of game
playGame :: Game
playGame dirct game =
  if not lost
    then ContinueGame newGame
  else EndOfGame newGame
    where
      lost = isLost newGame
      newGame = GameState newBoard2 gen
      gen = snd . next . seed $ game 
      newBoard2 = 
        if isChanged
          then randomInsert dirct (seed game) newBoard1
        else newBoard1
      -- isChanged = newBoard1 /= (board game)
      isChanged = not $ boardsEqual newBoard1 (board game)
      newBoard1 = shiftBoard dirct (board game)

-- It extract the grame state and identify its' continuable or not from given game result
gameContinue :: GameResult -> (GameState, Bool)
gameContinue (ContinueGame game) = (game, True)
gameContinue (EndOfGame game) = (game, False)

-- This function will play the gramw in CUI version
playCUI :: GameState -> IO GameState
playCUI game =
  do
    line <- getLine
    let result =if line == "L"
                  then playGame L game 
                else if line == "R"
                  then playGame R game
                else if line == "U"
                  then playGame U game
                else if line == "D"
                  then playGame D game
                else 
                  playGame L game
    
    let (newGame, cond) = gameContinue result
    print (board newGame)
    if cond
      then playCUI newGame
    else
      return newGame
      