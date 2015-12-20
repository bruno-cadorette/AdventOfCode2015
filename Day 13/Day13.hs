{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Either

import Data.List
import qualified Data.Map as Map

data Link = Link {from :: String, to :: String, cost :: Int} deriving (Show)

sumBy :: (Functor t, Num b, Foldable t) => (a -> b) -> t a -> b
sumBy f = sum . fmap f

pairWise :: [a] -> [(a,a)]
pairWise = zip <*> tail

orderedTuple a b = if a < b then (a, b) else (b, a)

getAllNodes :: [Link] -> [String]
getAllNodes = nub . concatMap (\x-> [from x, to x])

createCostIndex :: [Link] -> Map.Map (String, String) Int
createCostIndex = Map.fromList . map getKey
    where getKey x = (((from x),(to x)), cost x)
    
pathCost :: Map.Map (String, String) Int -> [String] -> Int
pathCost costIndex xs = sumBy cost $ pairWise xs
    where cost (x, y)= costIndex Map.! (x, y)

computeAllCosts links perms = map (\x -> (pathCost costIndex x) + (pathCost costIndex $ reverse x)) $ perms
    where 
        costIndex = createCostIndex links


happinessParser = do
    f <- (const id <$> string "gain") <|> (const negate <$> string "lose") 
    space
    n <- decimal
    return (f n)
    
preferenceParser = do
    from <- many letter_ascii
    string " would "
    cost <- happinessParser
    string " happiness units by sitting next to "
    to <- many letter_ascii
    char '.'
    return $ Link from to cost

parseFile = rights . map (parseOnly preferenceParser . B.pack) . lines



getRealPermutations links = map (Data.List.take n . cycle) $ permutations names
    where 
        names = getAllNodes links
        n = 1 + length names

addMyself links = links ++ (concatMap (\x -> [Link me x 0, Link x me 0]) $ getAllNodes links)
    where me = "Bruno"
        
main = do 
    links <- parseFile <$> readFile "inputDay13.txt"
    print $ maximum $ computeAllCosts links $ getRealPermutations links
    let part2Links = addMyself links
    print $ maximum $ computeAllCosts part2Links $ getRealPermutations part2Links