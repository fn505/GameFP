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
  DrawAll       -> drawAll (player gstate, enemies gstate, bullets gstate)
  DrawBullet    -> drawBullets(bullets gstate)
  -- ShowANumber n -> color green (text (show n))
  -- ShowAChar   c -> color green (text [c])
  -- ShowCircle -> let (x, y) = circlePos gstate
  --               in translate x y $ color blue (circle 50)

drawAll :: (Player , [Enemy], [Bullet]) -> Picture
drawAll (player, enemies, bullets) = pictures[drawPlayer player, drawEnemies enemies, drawBullets bullets]

drawPlayer :: Player -> Picture
drawPlayer (MkPlayer x y r d) = pictures[translate x y $ color green $ circle r] 

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy

drawEnemy :: Enemy -> Picture
drawEnemy (MkEnemy x y s r a ) = pictures[translate x y $ color red $ circle r] 

drawBullets :: [Bullet] -> Picture
drawBullets = pictures . map drawBullet

drawBullet :: Bullet -> Picture
drawBullet (MkBullet x y _ _) = pictures[translate (x-10) y $ color blue $ rectangleSolid 20 5]




