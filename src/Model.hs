-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss.Interface.IO.Game (Key)

data InfoToShow = ShowNothing
                | DrawPlayer
                | DrawEnemies
                | DrawAll
                | DrawBullet

data Lives= Zero | One | Two | Three
data Hitbox = MkHitbox
  {
    hitboxPos :: Point
  , xRadius :: Float
  , yRadius :: Float
  }deriving(Show, Eq, Read)
data Point = MkPoint 
  {
    xCor :: Float
  , yCor :: Float
  } deriving(Show, Eq, Read)
  

data Bullet = MkBullet
    {
      bulletPos :: Point
    , bulletXRadius :: Float
    , bulletYRadius :: Float
    , bulletSpeed :: Float
    , targetEnemy :: Bool
    } deriving (Show, Eq)
    
data Player = MkPlayer
    { playerPos :: Point -- beweegbaar  
    , playerRadius :: Float
    , dead :: Bool
    } deriving (Show, Eq) 

data Enemy = MkEnemy
    { enemyPos :: Point
    , speed :: Float -- voor nu een vaste waarde
    , enemyRadius :: Float
    , active :: Bool
    } deriving (Show, Eq) 

data InputHelper = MkInputHelper { 
  downKeys :: [Key] , 
  screenSize :: (Int,Int) , 
  mousePosition :: (Float,Float)
  }
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1.5

data GameState = GameState {
                  infoToShow  :: InfoToShow
                , player :: Player
                , enemies :: [Enemy]
                , elapsedTime :: Float
                , bullets :: [Bullet] 
                , score :: Int
                , inputHelper :: InputHelper
                , enemyShootTimer :: Float 
                , lives :: Lives
                 }

initialState :: GameState
initialState = GameState {
                      infoToShow = DrawAll
                    , player = MkPlayer (MkPoint 0 0)  10 False
                    , enemies = []
                    , elapsedTime = 0
                    , bullets = []
                    , score = 0
                    , inputHelper = MkInputHelper [] (400,400) (0,0)
                    , enemyShootTimer = 0
                    , lives = Three
                  }
