-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- randomY waarde tussen -150 en 150
generateRandomY :: RandomGen g => g -> (Float, g)
generateRandomY gen = randomR (-150, 150) gen

spawnEnemy :: Float ->Enemy
spawnEnemy enemyYpos= MkEnemy (MkPoint 180 enemyYpos) 5 10 True

spawnPlayerBullet :: Player -> Bullet
spawnPlayerBullet (MkPlayer pos r _ _)  = MkBullet (MkPoint ((xCor pos) + 2*r) (yCor pos)) 10 2.5 5 True

shootingCondition :: Enemy -> Player -> Bool
shootingCondition e@(MkEnemy posE _ _ _) p@(MkPlayer posP _ _ _) = let yDistance = abs ((yCor posE) - (yCor posP)) in yDistance < 75
spawnEnemyBullet :: Enemy ->  Bullet
spawnEnemyBullet (MkEnemy pos _ r _) = MkBullet (MkPoint ((xCor pos) - 2*r) (yCor pos)) 10 2.5 10 False
                                                                 
                                                                

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = 
  let status = gameStatus gstate
  in case status of
    Running -> do
      let updatedPlayer = updatePlayerMovement (inputHelper gstate) (player gstate)
      let collisionResult = identifyCollisions updatedPlayer (enemies gstate) (bullets gstate)
      
      gstate <- handleCollisions collisionResult gstate
      let updatedEnemies = moveEnemies (enemies gstate)
          updatedBullets = moveBullets(bullets gstate)
          updatedExplosions = updateExplosions secs (explosions gstate)
          updatedNotifications = updateNotifications secs (notifications gstate)
         

          


      if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
        then do -- spawn an enemy every 1.5 secs + update gamestate
        gen <- newStdGen -- maak een random generator aan
        let (randomY, _) = generateRandomY gen -- generate een (nieuwe) randomY waarde,
        let spawnedEnemy = spawnEnemy randomY -- maak enemy aan met de randomY waarde
        let newBullets = [spawnEnemyBullet e | e <- updatedEnemies, shootingCondition e  (player gstate)] ++ updatedBullets
        return $ gstate { enemies = spawnedEnemy : updatedEnemies
                          , elapsedTime = 0
                          , bullets = newBullets -- updatedBullets
                          , enemyShootTimer = 0
                          , explosions = updatedExplosions
                          , notifications = updatedNotifications }
        else return $ gstate { player = updatedPlayer
        , enemies = updatedEnemies -- don't spawn enemy before those 1.5 secs 
        , elapsedTime = elapsedTime gstate + secs
        , bullets = updatedBullets
        , enemyShootTimer = 0 
        , explosions = updatedExplosions
        , notifications = updatedNotifications} 
    _ -> return gstate

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
        Char 'p' -> let status = gameStatus gstate
                    in case status of 
                      Running -> gstate {gameStatus = Paused, infoToShow = DrawPauseScreen}
                      Paused  -> gstate {gameStatus = Running, infoToShow = DrawAll}
        SpecialKey KeyEnter -> let status = gameStatus gstate
                               in if (status == GameOver)
                                  then initialState
                                  else gstate
        _ -> gstate { inputHelper = updatedInputHelper }
-- als een key losgelaten wordt -> verwijderen uit downkeys
inputKey ih@(MkInputHelper keyList _ _) (EventKey key Up _ _) gstate = 
    let updatedInputHelper = removeKey ih key  
    in gstate { inputHelper = updatedInputHelper } 

inputKey (MkInputHelper keyList _ _) _ gstate = gstate -- Otherwise keep the same

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
movePlayer delta player@(MkPlayer pos _ _ _) = player {playerPos = MkPoint (xCor pos) (yCor pos + delta)}


moveEnemies :: [Enemy] -> [Enemy]
moveEnemies = map moveEnemy

moveEnemy :: Enemy -> Enemy
moveEnemy enemy@(MkEnemy pos s _ _) = enemy {enemyPos = MkPoint (xCor pos - s) (yCor pos)}

moveBullets :: [Bullet] -> [Bullet]
moveBullets = map moveBullet

moveBullet :: Bullet -> Bullet
moveBullet bullet@(MkBullet pos _ _ s targetEnemies) = if(targetEnemies)
                                                        then bullet {bulletPos = MkPoint (xCor pos + s) (yCor pos)}
                                                        else bullet {bulletPos = MkPoint (xCor pos - s) (yCor pos)}



class KeysPressed a where
  isKeyDown :: InputHelper -> a -> Bool 

instance KeysPressed Key where 
  isKeyDown ih k = k `elem` downKeys ih 

instance KeysPressed Char where 
  isKeyDown ih k = isKeyDown ih ((Char) k)

data Target = PlayerTarget | EnemyTarget deriving (Show, Eq) -- | NoTarget
data CollisionType = PlayerEnemyCollision | BulletEnemyCollision | BulletPlayerCollision | NoCollision deriving(Show, Eq)


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
  let playerEnemyCollision  = any (objectsCollision player) enemies
      bulletEnemyCollision  = any (\bullet -> any (objectsCollision bullet) enemies) bullets
      bulletPlayerCollision = any (`objectsCollision` player) bullets
  in if playerEnemyCollision
       then PlayerEnemyCollision
       else if bulletEnemyCollision
            then BulletEnemyCollision 
            else if bulletPlayerCollision
              then BulletPlayerCollision
              else NoCollision


handleCollisions :: CollisionType -> GameState -> IO GameState
handleCollisions PlayerEnemyCollision gstate = do 
  putStrLn "player enemy collison"

  updatedHighScore <- readScores "src/playerScores.txt" (score gstate)
  writeScores (score gstate) "src/playerScores.txt"

  let updatedLives = updateLives True (lives gstate)
  return gstate { gameStatus = GameOver, infoToShow = DrawGameOverScreen, highScore = updatedHighScore }

  

handleCollisions BulletEnemyCollision gstate = do
  putStrLn "bullet collision"

  let collidedBullets = filter (\bullet -> any (objectsCollision bullet) (enemies gstate)) (bullets gstate)
      collidedEnemies = filter (\enemy -> any (objectsCollision enemy) collidedBullets) (enemies gstate)

      updatedBullets = filter (`notElem` collidedBullets) (bullets gstate)
      updatedEnemies = filter (`notElem` collidedEnemies) (enemies gstate)

      newExplosions = [MkExplosion (enemyPos e) 20 5 2 True | e <- collidedEnemies]
  
      accumalatedScore = length collidedEnemies
  return gstate {bullets = updatedBullets, enemies = updatedEnemies, score = score gstate + accumalatedScore, explosions = explosions gstate ++ newExplosions }

handleCollisions BulletPlayerCollision gstate = do
  
  putStrLn "player hit by bullet"

  let collidedBullets = filter (\bullet -> objectsCollision (player gstate) bullet) (bullets gstate)
  
  let updatedLives = updateLives False (lives gstate) 
  let newNotifications = [MkNotification (playerPos (player gstate)) 0.5]

  let updatedBullets = filter (`notElem` collidedBullets) (bullets gstate) 
  
  if (updatedLives == Zero)
  then do 
    updatedHighScore <- readScores "src/playerScores.txt" (score gstate)
    writeScores (score gstate) "src/playerScores.txt"
    return gstate { gameStatus = GameOver, infoToShow = DrawGameOverScreen, highScore = updatedHighScore }
  else return gstate { bullets = updatedBullets, lives = updatedLives, notifications = notifications gstate ++ newNotifications}

handleCollisions NoCollision gstate = do
  return gstate 


updateLives :: Bool -> Lives -> Lives
updateLives collided lives | collided = Zero 
                           | otherwise = case lives of 
                            Three -> Two
                            Two -> One
                            One -> Zero
                            Zero -> Zero

 

updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs explosions = 
  [explosion { explosionRadius = explosionRadius explosion - (decreaseRadius explosion)
             , explosionTimer = explosionTimer explosion - secs
             , isSolid = not(isSolid explosion)}
             | explosion <- explosions 
             , explosionTimer explosion > secs
             , explosionRadius explosion >= 0]

updateNotifications :: Float -> [Notification] -> [Notification]
updateNotifications secs notifications = [notification {notifyTimer = notifyTimer notification - secs } | notification <-notifications, notifyTimer notification > secs]

readScores :: FilePath -> Int -> IO Int
readScores path currentScore = do
  input <- readFile path 
  let content       = lines(input)  -- content :: [String] ["2", "3"]
  let intContent    = map stringToInt content -- [Int]
  let highestScore  = (maximum (intContent))  -- Int
  if(currentScore > highestScore)
  then return currentScore
  else return highestScore

getHighScore :: Int -> Int
getHighScore n = id n


stringToInt :: String -> Int
stringToInt s = read s

intToString :: Int -> String
intToString n = show n


writeScores :: Int -> FilePath -> IO ()
writeScores playerScore path = 
  let stringScore = show(playerScore)
  in appendFile path ("\n" ++ stringScore)
  


