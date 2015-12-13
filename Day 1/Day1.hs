import Data.List

getVal '(' = 1
getVal ')' = -1
getVal _ = 0
        
day1A :: String -> Int
day1A = sum . map getVal

day1B :: String -> Maybe Int
day1B = elemIndex (-1) . scanl (+) 0 . map getVal
        
        

main = readFile "inputDay1.txt" >>= print . day1B