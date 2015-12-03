import Data.List
import Data.List.Split

formula = map read . splitOn "x"

wrappingPaper str = (minimum xs) + sum (map (*2) xs)
    where 
        [l, w, h] = formula str
        xs = [l*w, w*h, h*l]
        
day2A = sum . map wrappingPaper . lines

ribbon str = product xs + min2
    where 
        xs = formula str
        min2 = sum $ map (*2) $ take 2 $ sort xs
        
day2B = sum . map ribbon . lines