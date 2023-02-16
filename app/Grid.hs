module Grid where

import GameData


-- combine the two grids; second grid combines to first grid
-- return (Grid, Bool) where Grid is the result of combine or first grid if it fails and Bool is true
-- if it's success to combine or false
combineGrids :: Int -> Int -> Grid -> Grid -> (Grid, Bool) 
combineGrids x y g1 g2 =
  if g1Value == g2Value
    then (Grid (g1Value + g2Value) (x, y), True)
  else
    (Grid g1Value (x, y), False)
  where
    g1Value = value g1
    g2Value = value g2
