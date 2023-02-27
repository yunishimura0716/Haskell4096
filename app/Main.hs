{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Game
import System.Random
import GameData
import Grid
import Board
import UIConstant
import Render
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game hiding (shift)
import Debug.Trace


-- How many times we call onStep function in a second
fps :: Int
fps = 60

-- Drawing windor setting
window :: Display
window = InWindow "4096" (500, 500) (50, 50)

-- Background color of window
background :: Color
background = white

-- Util for printing the current board to the console
debugBoard :: GameState -> GameState
debugBoard game = GameState (traceShowId (board game)) (seed game)

-- The function called as a response of user input
-- It takes the action (dirction) from the user input and current game result to return next game result
onMove :: Event -> GameResult -> GameResult
onMove (EventKey (SpecialKey k) Down _ _) result
        | k == KeyLeft = f L result
        | k == KeyRight = f R result
        | k == KeyUp = f U result
        | k == KeyDown = f D result
        | otherwise = result
          where
      f dirct result'
        | not isContinue = result'
        -- | otherwise = playGame dirct (debugBoard game)
        | otherwise = playGame dirct game
        where
          (game, isContinue) = gameContinue result'
onMove _ result = result

-- The function called `fps` times in a second
-- Normally, it extract the current board and grids, then modify to show Bounce Effect
onStep :: Float -> GameResult -> GameResult
onStep step (ContinueGame gamestate) =
  ContinueGame GameState {
    board = [ [ f gridtile | gridtile <- row ] | row <- board gamestate ],
    seed = seed gamestate }
  where
    f (Grid val pos as scl prg prV prP)
      | val > 0 && scl < 1       = Grid val pos as (scl+step*2.5) prg prV prP
      | as == Move && prg < 1    = Grid val pos as scl (prg+step*4.75) prV prP
      | as == Move && prg >= 1   = Grid val pos End 1 1 val pos
      | as == Merge && prg < 1   = Grid val pos as scl (prg+step*4.75) prV prP
      | as == Merge && prg >= 1  = Grid val pos Grow 1 1 val pos
      | as == Grow && scl < 1.15 = Grid val pos as (scl+step*1.5) prg prV prP
      | as == Grow && scl >= 1.15= Grid val pos Shrink (scl-step*1.5) prg prV prP
      | as == Shrink && scl > 1  = Grid val pos Shrink (scl-step*1.5) prg prV prP
      | as == Shrink && scl <= 1 = Grid val pos End 1 prg prV prP
      | otherwise          = Grid val pos as scl prg prV prP
onStep _ result = result

resultRender :: GameResult -> Picture
resultRender result
  | not isContinue = pictures [translate (negate 100) 150 . scale 0.2 0.2 . text $ "You lose!!!!!", render (board game)]
  | otherwise = render (board game)
    where
      (game, isContinue) = gameContinue result

boardSelectIO :: IO Int
boardSelectIO =
  do
    putStrLn ("Press 0 (4*4) or 1 (5*5) > ")
    line <- getLine
    let result
          | (line == "0") = 
            do return 4
          | (line == "1") = do return 5
          | otherwise =
            do
              putStrLn "Wrong Input."
              r <- boardSelectIO
              return r
    v <- result
    return v

main :: IO ()
main =
  do
    let seed = mkStdGen 40
    -- Allow the users to choose which board they want to play
    putStrLn "Please choose board size: "
    bSize <- boardSelectIO
    let board = createBoard bSize
    -- print(board)
    -- play (GameState board seed)
    let initBoard = randomInsert L seed board
    let game = ContinueGame (GameState initBoard seed)
    play window background fps game resultRender onMove onStep
