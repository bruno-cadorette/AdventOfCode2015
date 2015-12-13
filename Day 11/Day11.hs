import Data.Char
import Data.List
import Debug.Trace

incChar x = chr $ ord x + 1
follow a b = ord b - ord a == 1
nextPassword = reverse . nextPassword' . reverse
    where 
        nextPassword' ('z':xs) = 'a':nextPassword' xs
        nextPassword' (x:xs) = (incChar x):xs
        nextPassword' [] = []

charIncreaseRule (a:b:c:xs) 
    |a `follow` b && b `follow` c = True
    |otherwise = charIncreaseRule (b:c:xs)
charIncreaseRule _ = False

notContainsRule str = not $ any (\x-> elem x str) $ ['i','o','l']

numberOfPairs (x:y:xs)
    |x == y = 1 + numberOfPairs xs
    |otherwise = numberOfPairs (y:xs)
numberOfPairs _ = 0

isValid str = charIncreaseRule str && notContainsRule str && twoPairsRule str

twoPairsRule str = numberOfPairs str >= 2

getPassword pwd = 
    if isValid pwd then pwd
    else getPassword $ nextPassword pwd