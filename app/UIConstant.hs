module UIConstant where

import Graphics.Gloss.Data.Color


-- The folowing constants and functions represents the values used in GUI drawing

gridSize :: Float 
gridSize = 50

dividerSize :: Float
dividerSize = 10

boardSlot :: Float
boardSlot = gridSize + dividerSize

scale1Digit :: Float
scale1Digit = 0.2

scale2Digit :: Float
scale2Digit = 0.18

scale3Digit :: Float
scale3Digit = 0.14

scale4Digit :: Float
scale4Digit = 0.12

pos1Digit :: (Float, Float)
pos1Digit = (negate 8, negate 10)

pos2Digit :: (Float, Float)
pos2Digit = (negate 12, negate 9)

pos3Digit :: (Float, Float)
pos3Digit = (negate 15, negate 7)

pos4Digit :: (Float, Float)
pos4Digit = (negate 18,negate 5)

fitGridToBoard4 :: Float
fitGridToBoard4 = negate (dividerSize*1.5 + gridSize*1.5)

fitGridToBoard5 :: Float
fitGridToBoard5 = negate (dividerSize*2.0 + gridSize*2.0)

boardBackground :: Color
boardBackground = greyN 0.8

slotBackground :: Color
slotBackground = greyN 0.95

txtWhite :: Color
txtWhite = white

txtBlack :: Color
txtBlack = black

gridColor :: Int -> Color
gridColor n
  | n == 2 = makeColorI 238 228 218 255   -- #eee4da
  | n == 4 = makeColorI 237 224 200 255   -- #ede0c8
  | n == 8 = makeColorI 242 177 121 255   -- #f2b179
  | n == 16 = makeColorI 245 149 99  255   -- #f59563
  | n == 32 = makeColorI 246 124 95  255   -- #f67c5f
  | n == 64 = makeColorI 246 94  59  255   -- #f65e3b
  | n == 128 = makeColorI 237 207 114 255   -- #edcf72
  | n == 256 = makeColorI 237 204 97  255   -- #edcc61
  | n == 512 = makeColorI 237 200 80  255   -- #edc850
  | n == 1024 = makeColorI 237 197 63  255   -- #edc53f
  | n == 2048 = makeColorI 237 194 46  255   -- #edc22e
  | n == 4096 = makeColorI 255 255 255 255  -- #ffffff
  | otherwise = makeColorI 255 255 255 0

