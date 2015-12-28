import Data.List
import Control.Monad
import Parser

type MoleculesTrans = [(String, [String])]

updateAt :: Int -> a -> [a] -> [a]
updateAt _     _ []     = []
updateAt 0     e (_:xs) = e:xs 
updateAt index e (x:xs) = x: updateAt (index - 1) e xs

updateToken :: [String]-> (String, [String]) -> [String]
updateToken base (token, remplacements) = concatMap (\i -> map (\r -> concat $ updateAt i r base) remplacements) $ elemIndices token base

buildMol :: Foldable t => t (String, [String]) -> [String] -> Int
buildMol remplacements base = length $ nub $ concatMap (updateToken base) remplacements

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst k v (x:xs)
    |k `isPrefixOf` (x:xs) = v ++ drop (length k) (x:xs)
replaceFirst _ _ _ = []

allRemplacements :: Eq a => [a] -> [a] -> [a] -> [[a]]
allRemplacements key v = go
    where
    go [] = []
    go (x:xs)
        |key `isPrefixOf` (x:xs) = replaceFirst key v (x:xs) : map (x:) (go xs)
        |otherwise = map (x:) $ go xs
        
reverseTrans :: [(a,[b])] -> [(b,a)]
reverseTrans = concatMap (\(k,xs)-> map (\x -> (x,k)) xs)

part1 :: MoleculesTrans -> String -> Int
part1 trans = buildMol trans . getAllMolecules

part2 :: MoleculesTrans -> String -> Int
part2 r = replaceAndCount 0 
    where 
        trans = reverseTrans r
        replaceAndCount :: Int -> String -> Int
        replaceAndCount i str
            |str == "e" = i
            |otherwise = head $ concatMap (\(k,vs) ->
                map (replaceAndCount (i + 1)) $ allRemplacements k vs str) trans

main :: IO()
main = do
    d <- getData <$> readFile "inputDay19.txt"
    case d of
        Right (r,b) -> do 
            print $ part1 r b
            print $ part2 r b
        Left str -> print str