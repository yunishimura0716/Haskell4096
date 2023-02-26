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
import Text.Read
import Data.Maybe


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
-- If a game can continue (meaning the the current gamestate was built with the ContinueGame constructor), then the animation effects are changed
--  otherwise, the original gamestate is returned.
onStep :: Float -> GameResult -> GameResult
onStep delta_t (ContinueGame gamestate) = 
  ContinueGame GameState { 
    board = [ [ processTile gridtile | gridtile <- row ] | row <- board gamestate ], 
    seed = seed gamestate }
  where
    processTile (Grid value position animState scaleFactor progressRatio prevVal prevPos)
      | value > 0 && scaleFactor < 1              = Grid value position animState (scaleFactor+delta_t*2.5) progressRatio prevVal prevPos
      | animState == Move && progressRatio < 1    = Grid value position animState scaleFactor (progressRatio+delta_t*4.75) prevVal prevPos
      | animState == Move && progressRatio >= 1   = Grid value position End 1 1 value position
      | animState == Merge && progressRatio < 1   = Grid value position animState scaleFactor (progressRatio+delta_t*4.75) prevVal prevPos
      | animState == Merge && progressRatio >= 1  = Grid value position Grow 1 1 value position
      | animState == Grow && scaleFactor < 1.15   = Grid value position animState (scaleFactor+delta_t*1.5) progressRatio prevVal prevPos
      | animState == Grow && scaleFactor >= 1.15  = Grid value position Shrink (scaleFactor-delta_t*1.5) progressRatio prevVal prevPos
      | animState == Shrink && scaleFactor > 1    = Grid value position Shrink (scaleFactor-delta_t*1.5) progressRatio prevVal prevPos
      | animState == Shrink && scaleFactor <= 1   = Grid value position End 1 progressRatio prevVal prevPos
      | otherwise          = Grid value position animState scaleFactor progressRatio prevVal prevPos
onStep _ result = result

-- Higher-order function that produces the rendering function with the appropriate offset for the game-over text 
--  (bigger boards -> greater offset for text)
resultRender :: Int -> (GameResult -> Picture)
resultRender n result
  | not isContinue = pictures [translate (negate 100) y' . scale 0.2 0.2 . text $ "You lose!!!!!", render (board game)]
  | otherwise = render (board game)
    where
      (game, isContinue) = gameContinue result
      y' = 20 + (fromIntegral n * gridSize + (fromIntegral n + 1) * dividerSize) / 2

--  get the user's choice of board size from the CLI
--  only sizes > 2 are accepted ; we impose no upper limit on board size, but individual machines do
getUserBoardSize :: IO Int
getUserBoardSize =
  do
    putStrLn "\nWhat size board would you like?"
    boardsize <- getLine
    let n = (readMaybe boardsize :: Maybe Int)
    if isNothing n
      then do
        putStrLn "That's not a valid number... try again (enter 4 or 5)"
        getUserBoardSize
    else if fromJust n < 2
      then do
        putStrLn "You will lose on the first try... choose a different number"
        getUserBoardSize
    else 
      return (fromJust n)

main :: IO ()
main =
  do
    n <- getUserBoardSize
    let seed = mkStdGen 40
    let board = createBoard n
    -- print(board)
    -- play (GameState board seed)
    let initBoard = randomInsert L seed board
    let game = ContinueGame (GameState initBoard seed)
    play window background fps game (resultRender n) onMove onStep

