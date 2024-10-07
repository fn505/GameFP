-- data ShipType = Player | Enemy
--     deriving Show
data Lives = Zero | One | Two | Three 
     deriving (Show, Eq)


data GameState = GameState 
    { score :: Int
    , lives :: Lives
    , gameOver :: Bool
    }

data Player = Player
    { playerX :: Int -- vast
    , playerY :: Int -- beweegbaar  
    } deriving (Show, Eq) 

data Enemy = Enemy 
    { enemyX :: Int -- beweegbaar
    , enemyY :: Int -- vast
    , speed :: Int -- voor nu een vaste waarde
    , alive :: Bool 
    }  deriving (Show, Eq)  

player1 :: Player
player1 = Player {playerX = 1, playerY = 2}

-- getMovement :: IO Char 


-- getMovement = do 
--             x <- getChar
--             if (x == 'w')
--                 then movePlayer 1 

movePlayer :: Int -> Player -> Player
movePlayer delta player = player {playerY = playerY player + delta}

enemy1 :: Enemy
enemy1 = Enemy {enemyX = 100, enemyY = 2, speed = 2, alive = True}

moveEnemy :: Enemy -> Enemy
moveEnemy enemy = enemy {enemyX = enemyX enemy - speed enemy}


-- compareHeights :: Player -> Enemy -> Bool

-- niet compleet
-- compareHeights player enemy | player{playerY} == enemy{enemyY}                                              = True
--                             | player{playerY} >= enemy{enemyY - 2} && player{playerY} >= enemy{enemyY + 2}  = True
--                             | otherwise                                                                     = False
