module Main (main) where

import Lib
import Game
import System.Random
import GameData
import Board

main :: IO GameState
main =
  do
    let seed = mkStdGen 40
    let board = createBoard 3
    print(board)
    play (GameState board seed)
