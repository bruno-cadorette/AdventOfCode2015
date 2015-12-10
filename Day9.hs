import Data.List
import qualified Data.Map as Map

data Link = Link {from :: String, to :: String, cost :: Int}

orderedTuple a b = if a > b then (a, b) else (b, a)

parseFile :: String -> [Link]
parseFile = map parseLine . lines

parseLine :: String -> Link
parseLine str = Link from to (read cost)
    where [from, _, to, _, cost] = words str
    
getAllCities :: [Link] -> [String]
getAllCities = nub . concatMap (\x-> [from x, to x])

createCostIndex :: [Link] -> Map.Map (String, String) Int
createCostIndex = Map.fromList . map getKey
    where getKey x = (orderedTuple (from x) (to x), cost x)
    
pathCost :: Map.Map (String, String) Int -> [String] -> Int
pathCost costIndex xs = sum $ (zipWith cost <*> tail) xs
    where cost x y= costIndex Map.! (orderedTuple x y)

compute f file = f $ map (pathCost costIndex) $ permutations cities
    where 
        path = parseFile file
        cities = getAllCities path
        costIndex = createCostIndex path

main = do
    file <- readFile "inputDay9.txt"
    
    print $ compute minimum file
    print $ compute maximum file
    