{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}


import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import Board
import UIConstant
import GameData
import TestConstant

instance Arbitrary AnimationState where 
  arbitrary = arbitraryBoundedEnum

-- instance Arbitrary Position where 
--   arbitrary = do
--     Positive x <- arbitrary
--     Positive y <- arbitrary
--     return $ (x, y)

-- this will need to be improved later... (so that prevValues can only differ by a y or x value)
instance Arbitrary Grid where 
  arbitrary = do 
    Positive value <- arbitrary
    position <- arbitrary
    animationState <- arbitrary
    scl <- arbitrary
    progress <- arbitrary
    prevValue <- arbitrary
    prevPosition <- arbitrary
    return $ Grid value position animationState scl progress prevValue prevPosition

main :: IO ()
main = hspec $ do

  -- -- first run "meta" tests (e.g. the tests on the testing functions)
  -- describe "TestConstant.boardNCorner" $ do
  --   it "returns a board with one tile of value <a> in corner <corner>" $ do
  --     boardNCorner UpperLeft 2 2 End `shouldBe` board_2_UL
  --     boardNCorner UpperRight 2 2 End `shouldBe` board_2_UR
  --     boardNCorner LowerLeft 2 2 End `shouldBe` board_2_LL
  --     boardNCorner LowerRight 2 2 End `shouldBe` board_2_LR

  --     boardNCorner UpperLeft 2 2 Move `shouldBe` board_2_UL_m
  --     boardNCorner UpperRight 2 2 Move `shouldBe` board_2_UR_m
  --     boardNCorner LowerLeft 2 2 Move `shouldBe` board_2_LL_m
  --     boardNCorner LowerRight 2 2 Move `shouldBe` board_2_LR_m

  -- test the shift board function (and all of its sub-functions)
  describe "Board.shiftBoard" $ do
    it "no changes to boards with a single tile on the same wall as the direction parameter (e.g. single tile on left wall won't shift left)" $ do 
      shiftBoard L board_2_UL `shouldBe` board_2_UL_m
      shiftBoard L board_2_LL `shouldBe` board_2_LL_m
      shiftBoard R board_2_UR `shouldBe` board_2_UR_m 
      shiftBoard R board_2_LR `shouldBe` board_2_LR_m 
      shiftBoard U board_2_UL `shouldBe` board_2_UL_m 
      shiftBoard U board_2_UR `shouldBe` board_2_UR_m 
      shiftBoard D board_2_LL `shouldBe` board_2_LL_m 
      shiftBoard D board_2_LR `shouldBe` board_2_LR_m

    it "only changes animation state if previously 'End'" $ do
      shiftBoard L board_2_UL_m `shouldBe` board_2_UL_m
      shiftBoard L board_2_LL_m `shouldBe` board_2_LL_m
      shiftBoard R board_2_UR_m `shouldBe` board_2_UR_m 
      shiftBoard R board_2_LR_m `shouldBe` board_2_LR_m 
      shiftBoard U board_2_UL_m `shouldBe` board_2_UL_m 
      shiftBoard U board_2_UR_m `shouldBe` board_2_UR_m 
      shiftBoard D board_2_LL_m `shouldBe` board_2_LL_m 
      shiftBoard D board_2_LR_m `shouldBe` board_2_LR_m

  describe "Board.boardSize" $ do
    it "returns the proper (graphical) size of the board given 4 or 5 grid tiles" $ do
      boardSize 4 `shouldBe` ((gridSize * 4 + dividerSize * 5))
      boardSize 5 `shouldBe` ((gridSize * 5 + dividerSize * 6))

    it "returns the proper (graphical) size for an *arbitrary* number of grid tiles" $
      property $ \n -> boardSize n == ((gridSize * fromIntegral n + dividerSize * (fromIntegral n+1)))

  -- describe "Board.boardFrame"

  -- describe "Board.render"

  describe "Board.flattenBoard" $ do 
    it "returns a list of all gridtiles, instead of 2d list, for arbitrary 2d lists of grids" $
      property $ \board -> (flattenBoard board) == (concat board)