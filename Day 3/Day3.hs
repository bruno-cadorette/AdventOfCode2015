import Control.Arrow
import Data.List
import qualified Data.Set as Set

move :: Num a => Char -> (a,a) -> (a,a) 
move '>' = first (+1)
move '<' = first (subtract 1)
move '^' = second (+1)
move 'v' = second (subtract 1)
move _ = id

uniqueHouses :: String -> Set.Set (Int, Int)
uniqueHouses = Set.fromList . scanl (flip move) (0,0)
mapTuple f (a,b) = (f a, f b)

separateTurns :: [a] -> ([a], [a])
separateTurns = mapTuple (map snd) . partition fst . zip (cycle [True, False])

day3A = Set.size . uniqueHouses

day3B xs = Set.size $ Set.union santa robo
   where (santa, robo) = mapTuple uniqueHouses $ separateTurns xs