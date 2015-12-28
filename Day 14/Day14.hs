{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.List
import Data.Ord
import Data.Either
import Debug.Trace

data ReindeerConfig = ReindeerConfig { name :: String, speed :: Int, speedTime :: Int, restTime :: Int } deriving (Show)
data Mode = Rest Int | Run Int  deriving (Show)
data Reindeer = Reindeer { config :: ReindeerConfig, mode :: Mode, distance :: Int, time :: Int }  deriving (Show)

initReindeer config = Reindeer config (Run (speedTime config)) 0 0

updateTime reindeer = reindeer { time = time reindeer + 1}

step reindeer =
    case mode reindeer of
        Rest 1 -> (updateTime reindeer) { mode = Run (speedTime $ config reindeer) }
        Rest x -> (updateTime reindeer) { mode = Rest (x - 1) }
        Run 0 ->  (updateTime reindeer) { mode = Rest (restTime (config reindeer) - 1) }
        Run x ->  (updateTime reindeer) { mode = Run (x -1), distance = distance reindeer + speed (config reindeer) }

reindeerParser = do
    name <- many letter_ascii
    string " can fly "
    speed <- decimal
    string " km/s for "
    speedTime <- decimal
    string " seconds, but then must rest for "
    restTime <- decimal
    return $ ReindeerConfig name speed speedTime restTime

reindeerParse = rights . map (parseOnly reindeerParser . B.pack) . lines
    
computeDistance :: ReindeerConfig -> Int -> Int
computeDistance reindeer time = distance $(\r -> trace (show r) $ r ) $ (iterate step (initReindeer reindeer)) !! time
        
frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

part1 time = map (\r -> computeDistance r time)

part2 time reindeers = maximum $ frequency $ map getName $ concatMap (maxWithEqualities (comparing distance)) $ race
    where 
        init = map initReindeer reindeers
        race =  Data.List.take time $ tail $ iterate (map step) init
        getName = name . config

        
maxWithEqualities :: (a -> a -> Ordering) -> [a] -> [a]
maxWithEqualities orderFunc = getFirsts . reverse . sortBy orderFunc
    where 
        getFirsts (x:xs) = x: Data.List.takeWhile (\y -> EQ == orderFunc x y) xs
        
        
test = map (step . initReindeer)

        
main = do 
    reindeers <- reindeerParse <$> readFile "inputDay14.txt" 
    print $ part1 2503 $ reindeers
    print $ part2 2503 $ reindeers