{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.List
import Data.Ord
import Data.Either

data ReindeerConfig = ReindeerConfig { name :: String, speed :: Int, speedTime :: Int, restTime :: Int } deriving (Show)
data Mode = Rest Int | Run Int  deriving (Show)
data Reindeer = Reindeer { config :: ReindeerConfig, mode :: Mode, distance :: Int }  deriving (Show)

initReindeer config = Reindeer config (Run (speedTime config)) 0

step reindeer = 
    case mode reindeer of
        Rest 0 -> reindeer { mode = Run (speedTime $ config reindeer) }
        Rest x -> reindeer { mode = Rest (x - 1) }
        Run 0 -> reindeer { mode = Rest (restTime $ config reindeer) }
        Run x -> reindeer { mode = Run (x -1), distance = distance reindeer + speed (config reindeer) }

reindeerParser = do
    name <- many letter_ascii
    string " can fly "
    speed <- decimal
    string " km/s for "
    speedTime <- decimal
    string " seconds, but then must rest for "
    restTime <- decimal
    return $ ReindeerConfig name speed speedTime restTime
    
computeDistance :: ReindeerConfig -> Int -> Int
computeDistance reindeer time = distance $ last $ Data.List.take time $ tail $ iterate step $ initReindeer reindeer
        
frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

comet = ReindeerConfig "comet" 14 10 127
dancer = ReindeerConfig "dancer" 16 11 162

part1 time = maximum . map (\r -> computeDistance r time)

part2 time reindeers = map (maxWithEqualities (comparing distance)) $ race
    where 
        init = map initReindeer reindeers
        next = map step
        race = Data.List.take time $ tail $ iterate next init

        
maxWithEqualities :: (a -> a -> Ordering) -> [a] -> [a]
maxWithEqualities orderFunc = getFirsts . sortBy orderFunc
    where 
        getFirsts (x:xs) = x: Data.List.takeWhile (\y -> EQ == orderFunc x y) xs
        
        
        
test time reindeer = map distance $ Data.List.take (time) $ tail $ iterate step $ initReindeer reindeer
reindeerParse = rights . map (parseOnly reindeerParser . B.pack) . lines
main = do 
    reindeers <- reindeerParse <$> readFile "inputDay14.txt" 
    print $ last $ test 100000 comet
    --print $ map (last .test 2503) reindeers
    --print $ part2 2503 reindeers