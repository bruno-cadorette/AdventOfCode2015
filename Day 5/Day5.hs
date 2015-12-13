import Data.List

countBy f = length . filter f
isVowel = flip elem ['a', 'e', 'i', 'o', 'u']
dontContainBadWords str = not $ any (\x -> isInfixOf x str) ["ab", "cd", "pq", "xy"]
isNiceStringPt1 str = 
    3 <= countBy isVowel str && (any (\x->length x >= 2) $ group str) && 
        dontContainBadWords str
isNiceStringPt2 str = containsPair str && containsTrio str
        
containsPair (x:y:xs) = if isInfixOf [x,y] xs then True else containsPair (y:xs)
containsPair _ = False


containsTrio (x:y:x':xs) 
    |x==x' = True
    |otherwise = containsTrio (y:x':xs) 
containsTrio _ = False

getAnswer predicate = print . countBy predicate . lines    
        
main = do
    file <- readFile "inputDay5.txt"
    getAnswer isNiceStringPt1 file
    getAnswer isNiceStringPt2 file