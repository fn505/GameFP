-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss.Interface.IO.Game (Key)

data InfoToShow = ShowNothing
                | DrawAll
                | DrawPauseScreen
                | DrawGameOverScreen
                deriving (Show,Eq)

data GameStatus = Running | Paused | GameOver deriving (Show,Eq)

data Notification = MkNotification
  {
    notifyPos :: Point
  , notifyTimer :: Float

  }deriving (Show,Eq)

data Explosion = MkExplosion 
  {
    explosionPos :: Point
  , explosionRadius :: Float
  , decreaseRadius :: Float
  , explosionTimer :: Float
  , isSolid :: Bool
  }deriving(Show, Eq, Read)

data Lives = Zero | One | Two | Three deriving(Show,Eq)

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
    { bulletPos :: Point
    , bulletXRadius :: Float
    , bulletYRadius :: Float
    , bulletSpeed :: Float
    , targetEnemy :: Bool
    } deriving (Show, Eq)
    
data Player = MkPlayer
    { playerPos :: Point  
    , playerRadius :: Float
    , wasHit :: Bool
    , dead :: Bool
    } deriving (Show, Eq) 

data Enemy = MkEnemy
    { enemyPos :: Point
    , speed :: Float 
    , enemyRadius :: Float
    , active :: Bool
    } deriving (Show, Eq) 

-- Stores the pressed keys
data InputHelper = MkInputHelper 
    { downKeys :: [Key] 
    , screenSize :: (Int,Int) 
    , mousePosition :: (Float,Float)
    } deriving (Show,Eq)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1.5

data GameState = GameState {
                  infoToShow  :: InfoToShow
                , player :: Player
                , enemies :: [Enemy]
                , elapsedTime :: Float
                , bullets :: [Bullet] 
                , score :: Int
                , highScore :: Int
                , inputHelper :: InputHelper
                , enemyShootTimer :: Float 
                , lives :: Lives
                , explosions :: [Explosion]
                , notifications :: [Notification]
                , gameStatus :: GameStatus
                 } deriving (Show,Eq)

initialState :: GameState
initialState = GameState {
                      infoToShow = DrawAll
                    , player = MkPlayer (MkPoint 0 0) 10 False False
                    , enemies = []
                    , elapsedTime = 0
                    , bullets = []
                    , score = 0
                    , highScore = 0
                    , inputHelper = MkInputHelper [] (400,400) (0,0)
                    , enemyShootTimer = 0
                    , lives = Three
                    , explosions = []
                    , notifications = []
                    , gameStatus = Running
                  }
