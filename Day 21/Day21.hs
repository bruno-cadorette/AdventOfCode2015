import Types

data Fighter = Fighter { hp :: Int, damage :: Int, armor :: Int } deriving (Show)

data Turn = Good | Bad

boss = Fighter 103 9 2

toFighter p = Fighter 100 (attack p) (defense p)

isDead fighter = hp fighter <= 0

dealDamage attacker defender = defender { hp = hp defender - max 1 (damage attacker - armor defender) }

canBeat _ b g
    |hp b <= 0 = True
    |hp g <= 0 = False
canBeat Good b g = canBeat Bad (dealDamage g b) g
canBeat Bad b g  = canBeat Good b (dealDamage b g)

canBeatBoss = canBeat Good boss

allHands = 
    [Hand (l, r) |
    l <- Nothing : map Just rings,
    r <- Nothing : map Just rings
    , l /= r]
    
allPlayers = do
    w <- weapons
    a <- armors
    h <- allHands
    return $ Player w a h
    
part1 = minimum $ map cost $ filter (canBeatBoss . toFighter) allPlayers
part2 = maximum $ map cost $ filter (not . canBeatBoss . toFighter) allPlayers


--main = print [canBeat (Fighter 12 7 2) (Fighter 8 5 5), canBeat boss (Fighter 100 7 4)]