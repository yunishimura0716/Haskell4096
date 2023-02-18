module Render where

import Graphics.Gloss 

class Model a where
	render :: a -> Picture