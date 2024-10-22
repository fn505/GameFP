-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | DrawPlayer
                | DrawEnemies
                | DrawAll
                | DrawBullet

data Bullet =  MkBullet
    {
      bulletX :: Float
    , bulletY :: Float
    , bulletSpeed :: Float
    , targetEnemy :: Bool
    }
data Player = MkPlayer
    { playerX :: Float -- vast
    , playerY :: Float -- beweegbaar  
    , playerRadius :: Float
    , dead :: Bool
    } deriving (Show, Eq) 

data Enemy = MkEnemy
    { enemyX :: Float -- vast
    , enemyY :: Float -- beweegbaar  
    , speed :: Float -- voor nu een vaste waarde
    , enemyRadius :: Float
    , active :: Bool
    } deriving (Show, Eq) 


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1.5

data GameState = GameState {
                  infoToShow  :: InfoToShow
                , player :: Player
                , enemies :: [Enemy]
                , elapsedTime :: Float
                , bullets :: [Bullet] 
                , score :: Int
                 
                 }

initialState :: GameState
initialState = GameState {
                      infoToShow = DrawAll
                    , player = MkPlayer 0 0 10 False
                    , enemies = []
                    , elapsedTime = 0
                    , bullets = []
                    , score = 0
                  }
