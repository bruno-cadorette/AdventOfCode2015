import Data.List

fillEggnog = filter (\xs -> sum xs == 150) . subsequences

part1 :: [Int] -> Int
part1 = length . fillEggnog

part2 xs = length $ filter (\x-> length x == minLength) subs
    where
        subs = fillEggnog xs
        minLength = minimum $ map length subs
main = do
    containers <- (map read . lines) <$> readFile "inputDay17.txt"
    print $ part1 containers
    print $ part2 containers