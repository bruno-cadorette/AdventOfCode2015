import Control.Arrow
import Data.Maybe
import Data.List

data SpellType = Instant | Effect Int deriving(Show, Eq)

data Magic = Magic { cost :: Int, spell :: Spell, spellType :: SpellType } deriving(Show)
data Spell = Missile Int | Drain Int Int | Shield Int | Poison Int | Recharge Int deriving(Show, Eq)

data Player = Player { playerHp :: Int, mana :: Int, playerArmor :: Int, magics :: [Magic], spentMana :: Int, magicHistory :: [Magic] } deriving(Show)
data Monster = Monster { monsterHp :: Int, monsterDamage :: Int } deriving(Show)
type Fight = (Player, Monster)
        
createPlayer :: Int -> Int -> Player
createPlayer h m = Player h m 0 [] 0 []

spells :: [Magic]
spells =  [poison, missile, shield, drain, recharge]
    where
        missile = Magic { cost = 53, spellType = Instant, spell = Missile 4 }
        drain = Magic { cost = 73, spellType = Instant, spell = Drain 2 2 }
        shield = Magic { cost = 113, spellType = Effect 6, spell = Shield 7 }
        poison = Magic { cost = 173, spellType = Effect 6, spell = Poison 3}
        recharge = Magic { cost = 229, spellType = Effect 5, spell = Recharge 101 }

canCast :: Player -> Magic -> Bool
canCast p magic = cost magic <= mana p && all (\m -> spell m /= spell magic) (magics p)

applySpell :: Fight -> Spell -> Fight
applySpell (p, m) (Missile x) = (p, m { monsterHp = monsterHp m - x })
applySpell (p, m) (Drain h x) = (p {playerHp = playerHp p + h}, m { monsterHp = monsterHp m - x })
applySpell (p, m) (Shield s) = (p {playerArmor = s}, m)
applySpell (p, m) (Poison x) = (p, m { monsterHp = monsterHp m - x })
applySpell (p, m) (Recharge x) = (p { mana = mana p + x}, m)

castMagic :: Fight -> Magic -> Fight
castMagic f magic = (p { mana = mana p - cost magic, spentMana = spentMana p + cost magic, magics = magicList, magicHistory = magic : magicHistory p}, m) 
    where
        ((p, m),magicList) = 
            case spellType magic of
                Instant -> (applySpell f $ spell magic, magics p)
                _       -> (f, magic:magics p)
                
monsterHit :: Fight -> Fight
monsterHit (p, m) = (p { playerHp = playerHp p - max 1 (monsterDamage m - playerArmor p) }, m)
        
fightResult :: Int -> Fight -> Either Int Fight
fightResult alpha f@(p, m)
    | playerHp p <= 0 || spentMana p >= alpha  = Left maxBound
    | monsterHp m <= 0 = Left $ spentMana p
    | otherwise        = Right f
    
playerCast :: Int -> Fight -> Int
playerCast alpha f@(p, _) = playerTurn $ filter (canCast p) spells
    where
        playerTurn [] = maxBound
        playerTurn xs = foldr (\x a -> min a $ bossFight a $ castMagic f x ) alpha xs
        
nextMagicTurn :: Magic -> Maybe Magic
nextMagicTurn (Magic  a b (Effect n)) 
    | n > 1     = Just $ Magic  a b (Effect (n-1))
nextMagicTurn _ = Nothing

removeEffect :: Fight -> Spell -> Fight
removeEffect (p, m)  (Shield _) = (p {playerArmor = 0}, m)
removeEffect f _ = f

spellEffect :: Fight -> Magic -> (Fight, Maybe Magic)
spellEffect f magic = (fight, m)
    where 
        m = nextMagicTurn magic
        s = spell magic
        fight =  case fmap spell m of
            Just _ -> applySpell f s
            Nothing -> removeEffect (applySpell f s) s
        
applyEffects :: Fight -> Fight
applyEffects fight@(p, _) = (p' { magics = ms }, m)
    where ((p', m), ms) = second catMaybes $ mapAccumR spellEffect fight $ magics p
        
bossFight :: Int -> Fight -> Int
bossFight alpha f = either id id $ do
    a <- fr $ applyEffects f
    b <- fr $ monsterHit a
    b' <- fr $ startPlayerTurn b
    c <- fr $ applyEffects  b'
    return $ playerCast alpha c
    where fr = fightResult alpha
    
startPlayerTurn :: Fight -> Fight
startPlayerTurn (p, m) =
 (p { playerHp = playerHp p - 1}, m)
        
startFight :: Fight -> Int
startFight = playerCast maxBound . startPlayerTurn

boss :: Monster
boss = Monster 55 8

main :: IO () 
main = do
    print $ startFight (createPlayer 10 250, Monster 13 8)
    print $ startFight (createPlayer 10 250, Monster 14 8)
    print $ startFight (createPlayer 50 500, boss)