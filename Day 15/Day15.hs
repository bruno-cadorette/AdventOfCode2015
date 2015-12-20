import Data.Matrix

type Cookie = ([Int], Int)
puzzleInput :: [Cookie]
puzzleInput = [([3,0,0,-3], 2),([-3,3,0,0], 9),([-1,0,4,0], 1), ([0,0,-2,2], 8)]

puzzleInput' = [([-1,-2,6,3], 8),([2,3,-2,-1],3)]

getIngredients :: [Cookie] -> Matrix Int
getIngredients = fromLists . fmap fst

getCalories :: [Cookie] -> Matrix Int
getCalories xs = fromList 1 (length xs) $ fmap snd xs

compute ing = maximum . map (\x -> product . fmap (max 0) $ multStd x ing)

possibleQuantities :: Int -> [Matrix Int]
possibleQuantities i = map (fromList 1 i) $ domain i 100
    where
        domain 1 m = map (\x -> [x]) [0..m]
        domain i m = concatMap(\x-> map (x:) $ filter (\xs -> x + sum xs == m) $ domain (i - 1) (m - x)) [0..m]


part1 ing = compute ing $ possibleQuantities (nrows ing)
part2 ing = compute ingredients $ filter (enoughCalories calories) $ possibleQuantities (nrows ingredients)
    where 
        ingredients = getIngredients ing
        calories = getCalories ing

enoughCalories :: Matrix Int -> Matrix Int -> Bool
enoughCalories caloriesMatrix = (== 500) . caloryCount caloriesMatrix
      
caloryCount :: Matrix Int -> Matrix Int -> Int      
caloryCount caloriesMatrix = sum . elementwise(*) caloriesMatrix

main = print $ part2 puzzleInput