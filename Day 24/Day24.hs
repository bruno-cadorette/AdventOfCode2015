import Data.List

qe :: Integer -> [Integer] -> Integer
qe n xs = minimum $ map product $ filter (\x -> length x == minL) xs'
    where 
        s = sum xs `div` n
        xs' = filter (\x -> sum x == s) $ subsequences xs
        minL = minimum $ map length xs'
        
main :: IO()   
main = do
    file <- readFile "inputDay24.txt"
    print $ qe 4 $ map read $ lines file
