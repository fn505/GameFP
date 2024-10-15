-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  DrawPlayer    -> drawPlayer (player gstate)
  DrawEnemies   -> drawEnemies (enemies gstate)
  DrawAll       -> drawAll (player gstate, enemies gstate)
  -- ShowANumber n -> color green (text (show n))
  -- ShowAChar   c -> color green (text [c])
  -- ShowCircle -> let (x, y) = circlePos gstate
  --               in translate x y $ color blue (circle 50)

drawAll :: (Player , [Enemy]) -> Picture
drawAll (player, enemies) = pictures[drawPlayer player, drawEnemies enemies]

drawPlayer :: Player -> Picture
drawPlayer (Player x y) = pictures[translate x y $ color green $ circle 10] 

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy x y s True) = pictures[translate x y $ color red $ circle 10] 