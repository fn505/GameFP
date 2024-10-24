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
drawPlayer (MkPlayer pos r d) = pictures[translate (xCor pos) (yCor pos) $ color green $ circle r] 

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy

drawEnemy :: Enemy -> Picture
drawEnemy (MkEnemy pos s r a ) = pictures[translate (xCor pos) (yCor pos) $ color red $ circle r] 

drawBullets :: [Bullet] -> Picture
drawBullets = pictures . map drawBullet

drawBullet :: Bullet -> Picture
drawBullet (MkBullet pos xr yr _ _) = pictures[translate ((xCor pos)-xr) (yCor pos) $ color blue $ rectangleSolid (2*xr) (2*yr)]




