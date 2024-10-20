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
  
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- Move the enemies
  let updatedEnemies = moveEnemies (enemies gstate)
  let movedBullet = moveBullet(bullet gstate)
  let collision = case infoToShow gstate of
                    DrawAll -> any (checkCollision (player gstate)) updatedEnemies
                    _       -> False

  if collision
    then do
      putStrLn "Collision"
      return gstate
    else if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
      then do
        newEnemy <- spawnEnemy
        return $ gstate { enemies = newEnemy : updatedEnemies, elapsedTime = 0, bullet = movedBullet }
      else return $ gstate { enemies = updatedEnemies
                           , elapsedTime = elapsedTime gstate + secs, bullet = movedBullet }


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
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

moveBullet :: Bullet -> Bullet
moveBullet bullet = bullet {bulletX = bulletX bullet - bulletSpeed bullet}

checkCollision :: Player -> Enemy -> Bool
checkCollision (Player px py pr _) (Enemy ex ey _ er _) =
    (ex + er > px - pr) && (ex - er < px + pr) &&
    (ey + er > py - pr) && (ey - er < py + pr)

