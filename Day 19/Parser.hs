{-# LANGUAGE OverloadedStrings #-}
module Parser(getData, getAllMolecules) where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8(pack)
import Data.List.Split
import Control.Applicative
import Text.Regex.Posix
import Control.Monad
import qualified Data.Map as Map

transformationEntryParser :: Parser (String, [String])
transformationEntryParser = do
    r1 <- many letter_ascii
    void $ string " => "
    r2 <- many letter_ascii
    return (r1, [r2])
    
transformationParser :: Parser [(String, [String])]
transformationParser = many $ transformationEntryParser <* endOfLine

getAllMolecules :: String -> [String]
getAllMolecules str = getAllTextMatches $ str =~ ("[A-Z][a-z]*" :: String)

getData :: String -> Either String ([(String, [String])], String)
getData str = fmap (\x -> (Map.assocs x, mol)) trans
    where 
        [transformationsRaw, [mol]] = splitOn [""] $ lines str
        trans = Map.fromListWith (++) <$> parseOnly transformationParser (pack $ unlines transformationsRaw)
        