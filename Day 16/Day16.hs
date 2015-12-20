{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8(pack)
import Data.Either
import Control.Applicative
import qualified Data.Map as Map

type Items = (Map.Map String Int)
data Aunt = Aunt { auntId :: Int, auntItems :: Items } deriving (Show)

auntItemParser = do
    name <- many letter_ascii
    string ": "
    quantity <- decimal
    return (name, quantity)
    
auntParser = do
    string "Sue "
    auntId <- decimal
    string ": "
    items <- Map.fromList <$> auntItemParser `sepBy` (string ", ") 
    return $ Aunt auntId items
    
mfcsamData = [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

removeWrongAunts :: (Int -> Int -> Bool) -> (String, Int) -> [Aunt] -> [Aunt]
removeWrongAunts isValid (itemName, itemQuantity) = filter (keep . Map.lookup itemName . auntItems)
    where
        keep Nothing = True
        keep (Just a) = isValid a itemQuantity
        
        
auntParse = rights . map (parseOnly auntParser . pack) . lines

part1 aunts = foldr (removeWrongAunts (==)) aunts mfcsamData
part2 aunts = foldr (\x -> removeWrongAunts (getValidation x) x) aunts mfcsamData

getValidation ("cats", _) = (>)
getValidation ("trees", _) = (>)
getValidation ("pomeranians", _) = (<)
getValidation ("goldfish", _) = (<)
getValidation (_, _) = (==)


main = do
    aunts <- auntParse <$> readFile "inputDay16.txt"
    print $ part1 aunts
    print $ part2 aunts