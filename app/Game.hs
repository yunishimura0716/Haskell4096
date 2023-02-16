module Game where

import System.Random
import Data.List
import GameData
import Board
import Grid

-- define game
type Game = Direction -> GameState -> GameResult

giveRandomElement :: StdGen -> [Int] -> Int
giveRandomElement generator giveList = giveList !! rand where
    n = length giveList
    (rand, _) = randomR (0,(n-1)) generator

-- insert function
insertFunc :: Int -> Int -> Int -> Board -> [Grid]
insertFunc randV chosenY i board =
  if i == chosenY
    then reverse ((Grid randV (x, chosenY)) : (tail (reverse grids)))
  else grids
    where
      x = length grids - 1
      grids = board !! i


-- get add blanks and a grid with random number (2 or 4)
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

-- play a game
playGame :: Game
playGame dirct game =
  if iscontinue
    then ContinueGame newGame
  else EndOfGame newGame
    where
      newGame = GameState newBoard2 gen
      gen = snd . next . seed $ game 
      newBoard2 = 
        if iscontinue
          then randomInsert dirct (seed game) newBoard1
        else newBoard1
      iscontinue = True
      newBoard1 = shiftBoard dirct (board game)
-- continue or dead
continue :: GameResult -> (GameState, Bool)
continue (ContinueGame game) = (game, True)
continue (EndOfGame game) = (game, False)

-- play
play :: GameState -> IO GameState
play game =
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
    
    let (newGame, cond) = continue result
    print (board newGame)
    if cond
      then play newGame
    else
      return newGame
      