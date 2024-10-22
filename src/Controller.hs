-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random



-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
--     do randomNumber <- randomIO
--        let newNumber = abs randomNumber `mod` 10
--        return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
--     return $ gstate { elapsedTime = elapsedTime gstate + secs }



spawnEnemy :: IO Enemy
spawnEnemy  = do
  enemyYpos <- randomRIO (-150, 150)
  return $ MkEnemy 180 enemyYpos 10 10 True

spawnPlayerBullet :: Player -> Bullet
spawnPlayerBullet player = MkBullet ((playerX player) + 2*playerRadius player) (playerY player) 5 False
  
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- Move the enemies
  let updatedEnemies = moveEnemies (enemies gstate)
  --moving bullets
  let updatedBullets = moveBullets(bullets gstate)
  --checks if any bullets in the gamestate hit any enemy
  let shotEnemy = any (\enemy -> any (`checkHitEnemy` enemy) updatedBullets) (enemies gstate)
  -- let collision = any(checkCollision(player gstate)) updatedEnemies

  if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    then do -- spawn an enemy every 1.5 secs + update gamestate
    newEnemy <- spawnEnemy
    return $ gstate { enemies = newEnemy : updatedEnemies, elapsedTime = 0, bullets = updatedBullets}
    else return $ gstate { enemies = updatedEnemies -- dont spawn enemy before those 1.5 secs 
                        , elapsedTime = elapsedTime gstate + secs, bullets = updatedBullets }


-- | Handle user input
input :: (Monad m) => Event -> GameState -> m GameState
input e gstate = return (inputKey (MkInputHelper [] (400,400) (0,0)) e gstate)

inputKey :: InputHelper -> Event -> GameState -> GameState
-- when you press the spacebar, a new bullet gets created at the place of the player + add the bullet to the gamestate
-- inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
--   = let newBullet = spawnPlayerBullet (player gstate)
--     in gstate {bullets = newBullet : bullets gstate}
-- inputKey (EventKey (Char 'w') Down _ _) gstate 
--   = gstate { player = movePlayer 10 (player gstate)}
-- inputKey (EventKey (Char 's') Down _ _) gstate
--   = gstate { player = movePlayer (-10) (player gstate)}

inputKey ih@(MkInputHelper keyList _ _) (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate
inputKey (MkInputHelper keyList _ _) _ gstate = gstate -- Otherwise keep the same

data InputHelper = MkInputHelper { 
  downKeys :: [Key] , 
  screenSize :: (Int,Int) , 
  mousePosition :: (Float,Float)
  }

-- class KeysPressed a where
--   isKeyDown :: InputHelper -> a -> Bool 

-- instance KeysPressed Key where 
--   isKeyDown ih k = k `elem` downKeys ih 

-- instance KeysPressed Char where 
--   isKeyDown ih k = isKeyDown ih . Char 


-- data MovingDirection = Down | NoMoving | MovingUp


-- getMovement :: KeysPressed => InputHelper -> MovingDirection
-- getMovement ih = 


addKey :: InputHelper -> Key -> InputHelper
addKey ih k = ih{downKeys = k : downKeys ih}

movePlayer :: Float -> Player -> Player
movePlayer delta player = player {playerY = playerY player + delta}


moveEnemies :: [Enemy] -> [Enemy]
moveEnemies = map moveEnemy

moveEnemy :: Enemy -> Enemy
moveEnemy enemy = enemy {enemyX = enemyX enemy - speed enemy}

moveBullets :: [Bullet] -> [Bullet]
moveBullets = map moveBullet

moveBullet :: Bullet -> Bullet
moveBullet bullet = bullet {bulletX = bulletX bullet + bulletSpeed bullet}

--check for collision between the player and the enemy
checkCollision :: Player -> Enemy -> Bool
checkCollision (MkPlayer px py pr _) (MkEnemy ex ey _ er _) =
    ((ex + er) >= px - pr) && ((ex - er) <= px + pr) &&
    ((ey + er) >= py - pr) && ((ey - er) <= py + pr)

--check if a bullet hits an enemy
checkHitEnemy :: Bullet -> Enemy -> Bool
checkHitEnemy (MkBullet bx by _ _) (MkEnemy ex ey _ er _) =
    ((ex + er) >= bx) && ((ex - er) <= bx) &&
    ((ey + er) >= by) && ((ey - er) <= by)


