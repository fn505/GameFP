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
  return $ Enemy 180 enemyYpos 10 10 True

spawnPlayerBullet :: Player -> Bullet
spawnPlayerBullet player = Bullet ((playerX player) + 2*playerRadius player) (playerY player) 5 False
  
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- Move the enemies
  let updatedEnemies = moveEnemies (enemies gstate)
  --moving bullets
  let movedBullets = moveBullets(bullets gstate)
  --checks if bullets in the gamestate hit an enemy
  let shotEnemy = any (\enemy -> any (`checkHitEnemy` enemy) movedBullets) (enemies gstate)

  if shotEnemy
    then do
      putStrLn "shot"
      return gstate
    else if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
      then do -- spawn an enemy every 1.5 secs + update gamestate
        newEnemy <- spawnEnemy
        return $ gstate { enemies = newEnemy : updatedEnemies, elapsedTime = 0, bullets = movedBullets}
      else return $ gstate { enemies = updatedEnemies -- dont spawn enemy before those 1.5 secs 
                           , elapsedTime = elapsedTime gstate + secs, bullets = movedBullets }


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- when you press the spacebar, a new bullet gets created at the place of the player + add the bullet to the gamestate
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
  = let newBullet = spawnPlayerBullet (player gstate)
    in gstate {bullets = newBullet : bullets gstate}
inputKey (EventKey (Char 'w') Down _ _) gstate
  = gstate { player = movePlayer 10 (player gstate)}
inputKey (EventKey (Char 's') Down _ _) gstate
  = gstate { player = movePlayer (-10) (player gstate)}
inputKey _ gstate = gstate -- Otherwise keep the same

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
checkCollision (Player px py pr _) (Enemy ex ey _ er _) =
    (ex + er > px - pr) && (ex - er < px + pr) &&
    (ey + er > py - pr) && (ey - er < py + pr)

--check if a bullet hits an enemy
checkHitEnemy :: Bullet -> Enemy -> Bool
checkHitEnemy (Bullet bx by _ _) (Enemy ex ey _ er _) =
    (ex + er >= bx) && (ex - er <= bx) &&
    (ey + er >= by) && (ey - er <= by)


