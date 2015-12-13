{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as Map

sumBy f = sum . fmap f

jsonSum f (Number n) = n
jsonSum f (Array arr)  = sumBy (jsonSum f) arr
jsonSum f (Object obj) = if f obj then sumBy (jsonSum f) $ Map.elems obj else 0
jsonSum f _            = 0

part1 = jsonSum (const True)
part2  = jsonSum (not . any (\(k,v)->k=="red" || valueIsRed v) . Map.toList)
    where 
        valueIsRed (String t) = t == "red"
        valueIsRed _ = False
        
main = do
    jsonData <- decode <$> B.readFile "inputDay12.txt"
    case jsonData of
        Just j-> do
            print $ part1 j
            print $ part2 j
        Nothing -> print 0