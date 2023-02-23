module Grid where

import GameData
import UIConstant
import Render
import Graphics.Gloss

-- grid needs to be redesigned to use the animation state:
-- if "move" find distances and put grid at right place
-- if "merge" render both (use move logic from above)
-- if "other" just scale it by s
-- 
-- 
-- 

instance Model Grid where
  render (Grid val (x, y) anis scl prog prevVal prevPos) 
    | anis == Move =
      gridRenderHelper val scl (x', y')
    | anis == Merge = 
      pictures [gridRenderHelper prevVal scl (x', y'), gridRenderHelper prevVal scl (x2, y2)]
    | otherwise    =
      gridRenderHelper val scl (size * (fromIntegral x) / scl, size * (fromIntegral y) / scl)
      where
        (x', y')
          | x2 == x1  = (x1, y1 + (y2 - y1) * prog)
          | otherwise = (x1 + (x2 - x1) * prog, y1)
        (x2, y2) = (size * (fromIntegral x) / scl, size * (fromIntegral y) / scl)
        (x1, y1) = (size * (fromIntegral (fst prevPos)) / scl, size * (fromIntegral (snd prevPos)) / scl)     
        size = gridSize + dividerSize

gridRenderHelper :: Int -> Float -> (Float, Float) -> Picture
gridRenderHelper n s (x, y) =
  scale s s . translate x y $ pictures [box, txt]
  where
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

-- combine the two grids; second grid combines to first grid
-- return (Grid, Bool) where Grid is the result of combine or first grid if it fails and Bool is true
-- if it's success to combine or false
combineGrids :: Int -> Int -> Grid -> Grid -> (Grid, Bool) 
combineGrids x y g1 g2 =
  if g1Value == g2Value
    then (Grid (g1Value + g2Value) (x, y) Merge 1 0 g2Value g2PrPos, True)
  else
    (Grid g1Value (x, y) Move 1 0 g1Value g1PrPos, False)
  where
    g2PrPos = prevPosition g2
    g1PrPos = prevPosition g1
    g1Value = value g1
    g2Value = value g2
