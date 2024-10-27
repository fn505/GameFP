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
spawnEnemy enemyYpos= MkEnemy (MkPoint 180 enemyYpos) 5 10 True

spawnPlayerBullet :: Player -> Bullet
spawnPlayerBullet (MkPlayer pos r _) = MkBullet (MkPoint ((xCor pos) + 2*r) (yCor pos)) 10 2.5 5 True

spawnEnemyBullet :: Enemy -> Bullet
spawnEnemyBullet (MkEnemy pos _ r _) = MkBullet (MkPoint ((xCor pos) - 2*r) (yCor pos)) 10 2.5 10 False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do

  let updatedPlayer = updatePlayerMovement (inputHelper gstate) (player gstate)
  -- Move the enemies

  --moving bullets
  --checks if any bullets in the gamestate hit any enemy
  --let shotEnemy = any (\enemy -> any (`checkHitEnemy` enemy) updatedBullets) (enemies gstate)
  let collisionResult = identifyCollisions updatedPlayer (enemies gstate) (bullets gstate)
  
  gstate <- handleCollisions collisionResult gstate
  let updatedEnemies = moveEnemies (enemies gstate)
      updatedBullets = moveBullets(bullets gstate)
      


  if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    then do -- spawn an enemy every 1.5 secs + update gamestate
    gen <- newStdGen -- maak een random generator aan
    let (randomY, _) = generateRandomY gen -- generate een (nieuwe) randomY waarde,
    let spawnedEnemy = spawnEnemy randomY -- maak enemy aan met de randomY waarde
    let newBullets = map spawnEnemyBullet updatedEnemies ++ updatedBullets
    return $ gstate { enemies = spawnedEnemy : updatedEnemies
                      , elapsedTime = 0
                      , bullets = newBullets -- updatedBullets
                      , enemyShootTimer = 0 }
    else return $ gstate { player = updatedPlayer
    , enemies = updatedEnemies -- don't spawn enemy before those 1.5 secs 
    , elapsedTime = elapsedTime gstate + secs
    , bullets = updatedBullets
    , enemyShootTimer = 0 }

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
            in gstate { bullets = newBullet : bullets gstate}
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
      step = 10
  in case playerMovement of 
    MovingUp -> movePlayer step player
    MovingDown -> movePlayer (-step) player
    NoMoving -> player

movePlayer :: Float -> Player -> Player
movePlayer delta player@(MkPlayer pos _ _) = player {playerPos = MkPoint (xCor pos) (yCor pos + delta)}


moveEnemies :: [Enemy] -> [Enemy]
moveEnemies = map moveEnemy

moveEnemy :: Enemy -> Enemy
moveEnemy enemy@(MkEnemy pos s _ _) = enemy {enemyPos = MkPoint (xCor pos - s) (yCor pos)}

moveBullets :: [Bullet] -> [Bullet]
moveBullets = map moveBullet

-- moveBullet :: Bullet -> Bullet
-- moveBullet bullet@(MkBullet pos _ _ s _) = bullet {bulletPos = MkPoint (xCor pos + s) (yCor pos)}

moveBullet :: Bullet -> Bullet
moveBullet bullet@(MkBullet pos _ _ s targetEnemies) = if(targetEnemies)
                                                        then bullet {bulletPos = MkPoint (xCor pos + s) (yCor pos)}
                                                        else bullet {bulletPos = MkPoint (xCor pos - s) (yCor pos)}



--check for collision between the player and the enemy
-- checkCollision :: Player -> Enemy -> Bool
-- checkCollision (MkPlayer px py pr _) (MkEnemy ex ey _ er _) =
--     ((ex + er) >= px - pr) && ((ex - er) <= px + pr) &&
--     ((ey + er) >= py - pr) && ((ey - er) <= py + pr)

--check if a bullet hits an enemy
-- checkHitEnemy :: Bullet -> Enemy -> Bool
-- checkHitEnemy (MkBullet bx by _ _) (MkEnemy ex ey _ er _) =
--     ((ex + er) >= bx) && ((ex - er) <= bx) &&

class KeysPressed a where
  isKeyDown :: InputHelper -> a -> Bool 

instance KeysPressed Key where 
  isKeyDown ih k = k `elem` downKeys ih 

instance KeysPressed Char where 
  isKeyDown ih k = isKeyDown ih ((Char) k)

data Target = PlayerTarget | EnemyTarget deriving (Show, Eq) -- | NoTarget
data CollisionType = PlayerEnemyCollision | BulletEnemyCollision | NoCollision deriving(Show, Eq)


class HasHitbox a where
  getHitbox :: a -> Hitbox
  getTarget :: a -> Target 

instance HasHitbox Player where
  getHitbox player = MkHitbox (playerPos player) (playerRadius player) (playerRadius player)
  getTarget player = EnemyTarget
  
instance HasHitbox Enemy where
  getHitbox enemy = MkHitbox (enemyPos enemy) (enemyRadius enemy) (enemyRadius enemy)
  getTarget enemy = PlayerTarget

instance HasHitbox Bullet where
  getHitbox bullet = MkHitbox (bulletPos bullet) (bulletXRadius bullet) (bulletYRadius bullet)
  getTarget bullet = if (targetEnemy bullet)
                        then EnemyTarget
                        else PlayerTarget
                      


hitboxCollision :: Hitbox -> Hitbox -> Bool
hitboxCollision h1 h2 = 
  let xCollision = ((xCor (hitboxPos h2) - (xRadius h2)) <= (xCor (hitboxPos h1) + (xRadius h1))) && ((xCor (hitboxPos h2) + (xRadius h2)) >= (xCor (hitboxPos h1) - (xRadius h1)))
      yCollision = ((yCor (hitboxPos h2) - (yRadius h2)) <= (yCor (hitboxPos h1) + (yRadius h1))) && ((yCor (hitboxPos h2) + (yRadius h2)) >= (yCor (hitboxPos h1) - (yRadius h1)))
  in
    xCollision && yCollision
  

objectsCollision :: (Eq a, Eq b, HasHitbox a,HasHitbox b) => a -> b -> Bool
objectsCollision ob1 ob2 = 
  let ob1Hitbox = getHitbox ob1
      ob2Hitbox = getHitbox ob2
      ob1Target = getTarget ob1
      ob2Target = getTarget ob2
  in if (ob1Target /= ob2Target)
        then hitboxCollision ob1Hitbox ob2Hitbox
        else False
  

identifyCollisions :: Player -> [Enemy] -> [Bullet] -> CollisionType
identifyCollisions player enemies bullets =
  let playerEnemyCollision = any (objectsCollision player) enemies
      bulletEnemyCollision = any (\bullet -> any (objectsCollision bullet) enemies) bullets
  in if playerEnemyCollision
       then PlayerEnemyCollision
       else if bulletEnemyCollision
            then BulletEnemyCollision 
            else NoCollision


handleCollisions :: CollisionType -> GameState -> IO GameState
handleCollisions PlayerEnemyCollision gstate = do 
  putStrLn "player enemy collison"
  return gstate 

handleCollisions BulletEnemyCollision gstate = do
  putStrLn "bullet collision"

  let collidedBullets = filter (\bullet -> any (objectsCollision bullet) (enemies gstate)) (bullets gstate)
  let collidedEnemies = filter (\enemy -> any (objectsCollision enemy) collidedBullets) (enemies gstate)

  let updatedBullets = filter (`notElem` collidedBullets) (bullets gstate)
  let updatedEnemies = filter (`notElem` collidedEnemies) (enemies gstate)

  return gstate { bullets = updatedBullets, enemies = updatedEnemies }


handleCollisions NoCollision gstate = do
  return gstate

