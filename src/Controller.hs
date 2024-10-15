-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do newEnemy <- spawnEnemy
       return $ gstate{enemies = newEnemy : enemies gstate , elapsedTime = 0 }
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

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

-- step :: Float -> GameState -> IO GameState
-- step _ gstate = return gstate -- No changes to the game state

spawnEnemy :: IO Enemy
spawnEnemy  = do
  enemyYpos <- randomRIO (-150, 150)
  return $ Enemy 180 enemyYpos 2 True

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _) gstate
  = gstate { player = movePlayer 2 (player gstate)}
inputKey (EventKey (Char 's') Down _ _) gstate
  = gstate { player = movePlayer (-2) (player gstate)}
inputKey _ gstate = gstate -- Otherwise keep the same

movePlayer :: Float -> Player -> Player
movePlayer delta player = player {playerY = playerY player + delta}


