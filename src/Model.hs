-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | DrawPlayer
                | DrawEnemies
                | DrawAll

data Player = Player
    { playerX :: Float -- vast
    , playerY :: Float -- beweegbaar  
    } deriving (Show, Eq) 

data Enemy = Enemy
    { enemyX :: Float -- vast
    , enemyY :: Float -- beweegbaar  
    , speed :: Float -- voor nu een vaste waarde
    , alive :: Bool
    } deriving (Show, Eq) 


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 3

data GameState = GameState {
                  infoToShow  :: InfoToShow
                , player :: Player
                , enemies :: [Enemy]
                , elapsedTime :: Float
                 
                 }

initialState :: GameState
initialState = GameState {
                      infoToShow = DrawAll
                    , player = Player 0 0
                    , enemies = [Enemy 180 0 10 True]
                    , elapsedTime = 0
                  }
