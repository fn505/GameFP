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
  DrawAll       -> drawAll (player gstate, enemies gstate, bullets gstate, lives gstate, score gstate, explosions gstate)
  DrawBullet    -> drawBullets(bullets gstate)
  -- ShowANumber n -> color green (text (show n))
  -- ShowAChar   c -> color green (text [c])
  -- ShowCircle -> let (x, y) = circlePos gstate
  --               in translate x y $ color blue (circle 50)

drawAll :: (Player , [Enemy], [Bullet], Lives, Int, [Explosion]) -> Picture
drawAll (player, enemies, bullets, lives, g, explosions) = pictures[drawPlayer player, drawEnemies enemies, drawBullets bullets,drawLives lives, drawScore g, drawExplosions explosions  ]

drawPlayer :: Player -> Picture
drawPlayer (MkPlayer pos r d) = pictures[translate (xCor pos) (yCor pos) $ color green $ circleSolid r] 

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy . filter active

drawEnemy :: Enemy -> Picture
drawEnemy (MkEnemy pos s r a ) = if (a == True)
  then do pictures[translate (xCor pos) (yCor pos) $ color red $ circleSolid r] 
  else mempty

drawBullets :: [Bullet] -> Picture
drawBullets = pictures . map drawBullet

-- drawBullet :: Bullet -> Picture
-- drawBullet (MkBullet pos xr yr _ _) = pictures[translate ((xCor pos)-xr) (yCor pos) $ color blue $ rectangleSolid (2*xr) (2*yr)]

drawBullet :: Bullet -> Picture
drawBullet (MkBullet pos xr yr _ targetEnemies) = if(targetEnemies) 
                                                    then pictures[translate ((xCor pos)-xr) (yCor pos) $ color blue $ rectangleSolid (2*xr) (2*yr)]
                                                    else pictures[translate ((xCor pos)-xr) (yCor pos) $ color yellow $ rectangleSolid (2*xr) (2*yr)]

drawExplosions :: [Explosion] -> Picture
drawExplosions = pictures . map drawExplosion

drawExplosion :: Explosion -> Picture
drawExplosion(MkExplosion pos r _ _ isSolid) = 
  if(isSolid)
    then pictures[translate (xCor pos) (yCor pos) $ color orange $ circleSolid r]
    else pictures[translate (xCor pos) (yCor pos) $ color orange $ circle r]

getLives :: Lives -> String
getLives lives = case lives of
                  Zero -> "0"
                  One -> "1"
                  Two -> "2"
                  Three -> "3"
-- later iets met screensize ipv magic number , ook zodat het meebeweegst als scherm groter wordt
drawLives :: Lives -> Picture
drawLives n = 
  let lives = getLives n
      liveDisplay = "Lives : " ++ lives
  in pictures [ translate (-190) 185 $ scaleText $ color green (text liveDisplay)] 

drawScore :: Int -> Picture
drawScore score = 
  let scoreDisplay = "Score : " ++ show(score)
  in pictures [ translate (0) 185 $ scaleText $ color green (text scoreDisplay)] 

scaleText :: Picture -> Picture
scaleText target = scale 0.1 0.1 target
