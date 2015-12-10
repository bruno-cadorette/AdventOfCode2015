{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Foldable
import Data.Functor
import Data.Array.MArray
import Data.Array.IO
import qualified Data.Map.Strict as Map

data Action = TurnOn | TurnOff | Toggle deriving (Show)    
type Position = (Int, Int)   
type Instruction = (Action, Position, Position)
type LightArray = IOUArray Position Int
    
actionParser = 
    (string "turn on" >> return TurnOn) <|>
    (string "turn off">> return TurnOff)<|>
    (string "toggle"  >> return Toggle)
    
coordonateParser = do
    a <- decimal 
    char ','
    b <- decimal
    return (a, b)

instructionParser = do
    action <- actionParser
    space
    from <- coordonateParser
    string " through "
    to <- coordonateParser
    return (action, from, to)
    
parseFile :: String -> Either String [Instruction]
parseFile = mapM (parseOnly instructionParser . B.pack) . lines

getRange :: Position -> Position -> [Position]
getRange (fromX, fromY) (toX, toY) = [(i,j) | i<-[fromX..toX], j<-[fromY..toY]]

update :: (Int -> Int) -> LightArray -> Position -> IO()
update func array pos = func <$> readArray array pos >>= writeArray array pos

updateValue :: Action -> LightArray -> Position -> IO ()
updateValue TurnOn = update (+1)
updateValue TurnOff = update (\x -> max (x-1) 0)
updateValue Toggle = update (+2)
    

updateLightArray :: LightArray -> Instruction -> IO ()
updateLightArray array (action, from, to) =
    mapM_ (updateValue action array) (getRange from to)
    

main = do
    file <- readFile "inputDay6.txt"
    
    case parseFile file of 
        Left err -> print err
        Right instructions -> do 
            lightMap <- newArray ((0,0),(999,999)) 0
            mapM_ (updateLightArray lightMap) instructions
            elems <- getElems lightMap
            print $ sum elems
    