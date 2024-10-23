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



-- spawnEnemy :: IO Enemy
-- spawnEnemy  = do
--   enemyYpos <- randomRIO (-150, 150)
--   return $ MkEnemy 180 enemyYpos 10 10 True

-- randomY waarde tussen -150 en 150
generateRandomY :: RandomGen g => g -> (Float, g)
generateRandomY gen = randomR (-150, 150) gen

spawnEnemy :: Float ->Enemy
spawnEnemy enemyYpos= MkEnemy 180 enemyYpos 10 10 True

spawnPlayerBullet :: Player -> Bullet
spawnPlayerBullet player = MkBullet ((playerX player) + 2*playerRadius player) (playerY player) 5 False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do

  let updatedPlayer = updatePlayerMovement (inputHelper gstate) (player gstate)
  -- Move the enemies
  let updatedEnemies = moveEnemies (enemies gstate)
  --moving bullets
  let updatedBullets = moveBullets(bullets gstate)
  --checks if any bullets in the gamestate hit any enemy
  --let shotEnemy = any (\enemy -> any (`checkHitEnemy` enemy) updatedBullets) (enemies gstate)
  -- let collision = any(checkCollision(player gstate)) updatedEnemies

  if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    then do -- spawn an enemy every 1.5 secs + update gamestate
      gen <- newStdGen -- maak een random generator aan
      let (randomY, _) = generateRandomY gen -- generate een (nieuwe) randomY waarde,
      let spawnedEnemy = spawnEnemy randomY -- maak enemy aan met de randomY waarde
      return $ gstate { enemies = spawnedEnemy : updatedEnemies
                        , elapsedTime = 0
                        , bullets = updatedBullets }
    else return $ gstate { player = updatedPlayer
                         , enemies = updatedEnemies -- don't spawn enemy before those 1.5 secs 
                         , elapsedTime = elapsedTime gstate + secs
                         , bullets = updatedBullets }

-- | Handle user input
input :: (Monad m) => Event -> GameState -> m GameState
input e gstate = 
  let ih = inputHelper gstate
  in return (inputKey ih e gstate)

inputKey :: InputHelper -> Event -> GameState -> GameState
-- in general -> als een key down is voeg het toe aan downkeys, daarna check for keyspace -> bullet toevoegen
-- verder gewone gstate teruggeven
inputKey ih@(MkInputHelper keyList _ _) (EventKey key Down _ _) gstate = 
    let updatedInputHelper = addKey ih key
         in case key of
        SpecialKey KeySpace -> 
            let newBullet = spawnPlayerBullet (player gstate)
            in gstate { bullets = newBullet : bullets gstate, inputHelper = updatedInputHelper }
        --later uitbreiden met pause en resume    
        _ -> gstate { inputHelper = updatedInputHelper }
-- als een key losgelaten wordt -> verwijderen uit downkeys
inputKey ih@(MkInputHelper keyList _ _) (EventKey key Up _ _) gstate = 
    let updatedInputHelper = removeKey ih key  
    in gstate { inputHelper = updatedInputHelper } 

inputKey (MkInputHelper keyList _ _) _ gstate = gstate -- Otherwise keep the same


-- oude code inputkey
-- when you press the spacebar, a new bullet gets created at the place of the player + add the bullet to the gamestate
-- inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
--   = let newBullet = spawnPlayerBullet (player gstate)
--     in gstate {bullets = newBullet : bullets gstate}
-- inputKey (EventKey (Char 'w') Down _ _) gstate 
--   = gstate { player = movePlayer 10 (player gstate)}
-- inputKey (EventKey (Char 's') Down _ _) gstate
--   = gstate { player = movePlayer (-10) (player gstate)}

-- inputKey ih@(MkInputHelper keyList _ _) (EventKey (SpecialKey KeySpace) Down _ _) gstate = let newBullet = spawnPlayerBullet (player gstate)
--     in gstate { bullets = newBullet : bullets gstate }

-- key toevoegen aan downkeys
addKey :: InputHelper -> Key -> InputHelper
addKey ih k = ih{downKeys = k : downKeys ih}

--key verwijderen uit downkeys
removeKey :: InputHelper -> Key -> InputHelper
removeKey ih k = ih { downKeys = filter (/= k) (downKeys ih) }

class KeysPressed a where
  isKeyDown :: InputHelper -> a -> Bool 

instance KeysPressed Key where 
  isKeyDown ih k = k `elem` downKeys ih 

instance KeysPressed Char where 
  isKeyDown ih k = isKeyDown ih ((Char) k)

data MovingDirection = MovingDown | NoMoving | MovingUp

-- pattermatching voor welke key down is
getMovement :: InputHelper -> MovingDirection
getMovement ih | isKeyDown ih (Char 'w') = MovingUp
               | isKeyDown ih (Char 's') = MovingDown
               | otherwise = NoMoving

-- de moveplayer methode aanroepen bij de bijbehorende movements
updatePlayerMovement :: InputHelper -> Player -> Player
updatePlayerMovement ih player =
  let playerMovement = getMovement ih
  in case playerMovement of 
    MovingUp -> movePlayer 10 player
    MovingDown -> movePlayer (-10) player
    NoMoving -> player

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


