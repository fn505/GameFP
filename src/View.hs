-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing     -> blank
  DrawAll         -> drawAll (player gstate, enemies gstate, bullets gstate, lives gstate, score gstate, explosions gstate, notifications gstate)
  DrawPauseScreen -> drawPauseScreen 
  DrawGameOverScreen -> drawGameOverScreen (highScore gstate) (score gstate)


drawAll :: (Player , [Enemy], [Bullet], Lives, Int, [Explosion], [Notification]) -> Picture
drawAll (player, enemies, bullets, lives, g, explosions, notifications) = pictures[drawPlayer player, drawEnemies enemies, drawBullets bullets,drawLives lives, drawScore g, drawExplosions explosions, drawNotifications notifications  ]

drawPauseScreen :: Picture
drawPauseScreen = 
  let pauseString = "Paused"
      resumeString = "Press P to resume the game"
  in pictures [ translate (-120) 0 $ scaleText 0.5 $ color green (text pauseString), translate (-160) (-100) $ scaleText 0.15 $ color green (text resumeString )]

drawGameOverScreen :: Int -> Int -> Picture
drawGameOverScreen highscore score = 
  let gameOverString = "Game Over"
      newGameString = "Press ENTER to start a new game"
  in pictures [ translate (-175) 0 $ scaleText 0.5 $ color red (text gameOverString),  translate (-170) (-100) $ scaleText 0.15 $ color red (text newGameString ), drawHighScore highscore, drawScore score ]


drawHighScore :: Int -> Picture
drawHighScore highscore =   
  let highscoreDisplay = "High Score : " ++ show(highscore)
  in pictures [ translate (-190) 185 $ scaleText 0.1 $ color green (text highscoreDisplay)] 


drawNotifications :: [Notification] -> Picture
drawNotifications = pictures . map drawNotification

drawNotification :: Notification -> Picture
drawNotification (MkNotification pos _) = 
  let notifString = "HIT"
  in pictures [translate (xCor pos) (yCor pos) $ scaleText 0.15 $ color white (text notifString)]

drawPlayer :: Player -> Picture
drawPlayer (MkPlayer pos r h d) = pictures[translate (xCor pos) (yCor pos) $ color green $ circleSolid r] 

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy . filter active

drawEnemy :: Enemy -> Picture
drawEnemy (MkEnemy pos s r a ) = if (a == True)
  then do pictures[translate (xCor pos) (yCor pos) $ color red $ circleSolid r] 
  else mempty

drawBullets :: [Bullet] -> Picture
drawBullets = pictures . map drawBullet

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

drawLives :: Lives -> Picture
drawLives n = 
  let lives = getLives n
      liveDisplay = "Lives : " ++ lives
  in pictures [ translate (-190) 185 $ scaleText 0.1 $ color green (text liveDisplay)] 

drawScore :: Int -> Picture
drawScore score = 
  let scoreDisplay = "Score : " ++ show(score)
  in pictures [ translate (0) 185 $ scaleText 0.1 $ color green (text scoreDisplay)] 

scaleText :: Float -> Picture -> Picture
scaleText factor target = scale factor factor target
