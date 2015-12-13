import Data.List
lookAndSay = concatMap (\x -> show (length x)  ++ [head x]) . group
applyLAS n base = length $ foldr (const lookAndSay) base [1..n]