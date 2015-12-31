module Types(Cost, Stats(..), Player(..), Hand(..), Armor(..), ItemType(..), Ring(..), Weapon(..), weapons, armors, rings) where
import Data.Maybe

type Cost = Int

class Stats a where
    cost :: a -> Int
    attack :: a -> Int
    defense :: a -> Int

data Weapon = Weapon Int Cost deriving(Show)
instance Stats Weapon where
    cost (Weapon _ x) = x
    attack (Weapon x _) = x
    defense _ = 0

data Armor = Armor Int Cost deriving(Show)
instance Stats Armor where
    cost (Armor _ x) = x
    attack _ = 0
    defense (Armor x _ )= x

data ItemType = Attack Int | Defense Int deriving(Show, Eq)

data Ring = Ring ItemType Cost deriving(Show, Eq)

instance Stats Ring where
    cost (Ring _ x) = x
    attack (Ring (Attack x) _)  = x
    attack _ = 0
    defense (Ring (Defense x) _) = x
    defense _ = 0

data Hand = Hand (Maybe Ring, Maybe Ring) deriving(Show)
computeHand f (left, right) = maybe 0 f left + maybe 0 f right
instance Stats Hand where
    cost (Hand hand) = computeHand cost hand
    attack (Hand hand) = computeHand attack hand
    defense (Hand hand) = computeHand defense hand

data Player = Player Weapon Armor Hand deriving(Show)
instance Stats Player where
    cost (Player weapon armor hand) = cost weapon + cost armor + cost hand
    attack (Player weapon armor hand) = attack weapon + attack armor + attack hand
    defense (Player weapon armor hand) = defense weapon + defense armor + defense hand
    
weapons = [(Weapon 4 8), (Weapon 5 10),(Weapon 6 25),(Weapon 7 40),(Weapon 8 74)]
armors = [(Armor 0 0), (Armor 1 13), (Armor 2 31), (Armor 3 53), (Armor 4 75), (Armor 5 102)]
rings = [(Ring (Attack 1) 25), (Ring (Attack 2) 50), (Ring (Attack 3) 100), (Ring (Defense 1) 20), (Ring (Defense 2) 40), (Ring (Defense 3) 80)]