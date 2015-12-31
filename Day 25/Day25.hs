nextCode :: Int -> Int
nextCode n = n * 252533 `rem` 33554393

locate n' a = (n * (n-1)) `div` 2 + a - 1
    where n = n' + (a - 1)

main = print $ iterate nextCode 20151125 !! locate 2978 3083