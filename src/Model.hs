-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | DrawPlayer
                | DrawEnemies
                | DrawAll
                | DrawBullet

data Bullet =  Bullet
    {
      bulletX :: Float
    , bulletY :: Float
    , bulletSpeed :: Float
    }
data Player = Player
    { playerX :: Float -- vast
    , playerY :: Float -- beweegbaar  
    , playerRadius :: Float
    , dead :: Bool
    } deriving (Show, Eq) 

data Enemy = Enemy
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
                , bullet :: Bullet 
                 
                 }

initialState :: GameState
initialState = GameState {
                      infoToShow = DrawBullet
                    , player = Player 0 0 10 False
                    , enemies = []
                    , elapsedTime = 0
                    , bullet = Bullet 20 20 10
                  }
