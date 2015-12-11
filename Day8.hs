import Data.Char
innerStringLength :: String -> Int
innerStringLength ('\\':'\\':xs) = 1 + innerStringLength xs
innerStringLength ('\\':'"':xs) = 1 + innerStringLength xs
innerStringLength ('\\':'x':xs) = 1 + (innerStringLength $ drop numberToDrop xs )
    where numberToDrop = min 2 (length $ takeWhile isHexDigit xs)
innerStringLength (x:xs) = 1 + innerStringLength xs
innerStringLength [] = -2

stringDifference x = length x - innerStringLength x

main = do
    file <- readFile "inputDay8.txt"
    print $ sum $ map stringDifference $ lines file
    print $ sum $ map (stringDifference . show) $ lines file