module Grid where

import GameData
import UIConstant
import Render
import Graphics.Gloss

-- Render for Grid
instance Model Grid where
  render (Grid n (x,y) _ scl)
    | n == 0 = blank
    | otherwise = scale scl scl . translate (x'/scl) (y'/scl) $ (pictures [box, txt])
      where
        x' = fromIntegral x * size
        y' = fromIntegral y * size
        size = gridSize + dividerSize
        box = color boxColor $ rectangleSolid gridSize gridSize
        txt = translate (fst txtPos) (snd txtPos) . scale txtScale txtScale . color txtColor . text . show $ n
        txtColor
          | n == 2 || n == 4 || n == 4096 = txtBlack
          | otherwise = txtWhite
        boxColor = gridColor n 
        (txtPos, txtScale)
          | n == 2 || n == 4 || n == 8 = (pos1Digit, scale1Digit)
          | n == 16 || n == 32 || n == 64 = (pos2Digit, scale2Digit)
          | n == 128 || n == 256 || n == 512 = (pos3Digit, scale3Digit)
          | n == 1024 || n == 2048 || n == 4096 || n == 8192 = (pos4Digit, scale4Digit)
          | otherwise = ((0,0),0)


-- Combine the two grids; second grid combines to first grid
-- Return (Grid, Bool) where Grid is the result of combine or first grid if it fails and Bool is true
-- if it's success to combine or false
combineGrids :: Int -> Int -> Grid -> Grid -> (Grid, Bool) 
combineGrids x y g1 g2 =
  if g1Value == g2Value
    then (Grid (g1Value + g2Value) (x, y) Grow 1, True)
  else
    (Grid g1Value (x, y) End 1, False)
  where
    g1Value = value g1
    g2Value = value g2
