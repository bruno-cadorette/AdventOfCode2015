import Data.List
import qualified Data.Set as Set
import Math.NumberTheory.Primes.Factorisation

factors :: Int -> [Int]
factors n = nub $ concatMap(\i -> [i, n `div` i]) $ filter (\i -> n `rem` i == 0) [1..(n `div` 2)]

part1 numberToFind = find (\i -> divisorSum  i >= 3400000) [1..]
    where n = numberToFind `div` 10
    
part2 numberToFind = find (\i -> realSum i >= n) [1..]
    where 
        n = numberToFind `div` 11
        realSum currentNumber = sum $ Set.filter (\i->i * 50 >= currentNumber) $ divisors currentNumber
    
main = print $ part2 34000000