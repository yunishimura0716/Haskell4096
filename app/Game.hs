module Game where

import System.Random
import Data.List
import GameData
import Board
import Grid
import UIConstant
import Render

instance Model GameState where
	render g = render $ board g

giveRandomElement :: StdGen -> [Int] -> Int
giveRandomElement generator giveList = giveList !! rand where
    n = length giveList
    (rand, _) = randomR (0,(n-1)) generator

-- insert a random Grid to the board 
insertFunc :: Int -> Int -> Int -> Board -> [Grid]
insertFunc randV chosenY i board =
  if i == chosenY
    then reverse ((Grid randV (x, chosenY) End 0) : (tail (reverse grids)))
  else grids
    where
      x = length grids - 1
      grids = board !! i


-- insert a grid with random number (2 or 4)
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

isLost :: GameState -> Bool
isLost game =
  b == left_b && b == right_b && b == up_b && b == down_b
    where
      down_b = shiftBoard D b
      up_b = shiftBoard U b
      right_b = shiftBoard R b
      left_b = shiftBoard L b
      b = board game

-- define game
type Game = Direction -> GameState -> GameResult

-- play a game
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
      isChanged = newBoard1 /= (board game)
      newBoard1 = shiftBoard dirct (board game)
-- continue, transition, or dead
gameContinue :: GameResult -> (GameState, Bool)
gameContinue (ContinueGame game) = (game, True)
gameContinue (EndOfGame game) = (game, False)

-- play
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
      