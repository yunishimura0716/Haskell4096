module Main (main) where

import Lib
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


fps :: Int
fps = 60

window :: Display
window = InWindow "4096" (500, 500) (50, 50)

background :: Color
background = white

debugBoard :: GameState -> GameState
debugBoard game = GameState (traceShowId (board game)) (seed game)

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

main :: IO ()
main =
  do
    let seed = mkStdGen 40
    let board = createBoard 4
    -- print(board)
    -- play (GameState board seed)
    let initBoard = randomInsert L seed board
    let game = (ContinueGame (GameState initBoard seed))
    play window background fps game resultRender onMove onStep
