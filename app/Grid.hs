module Grid where

import GameData


-- combine the two grids; second grid combines to first grid
-- return (Grid, Bool) where Grid is the result of combine or first grid if it fails and Bool is true
-- if it's success to combine or false
combineGrids :: Grid -> Grid -> (Grid, Bool) 
combineGrids _ _ = (Grid 0 (0, 0), True)