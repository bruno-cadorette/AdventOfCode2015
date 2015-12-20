import Control.Arrow
import Data.Array
import Data.Ix
import Data.List.Split(chunksOf)
data Light = On | Off deriving (Show)

toNumber On  = 1
toNumber Off = 0

fromLetter '#' = On
fromLetter '.' = Off

toLetter On  = '#'
toLetter Off = '.'

type ConwayBoard = Array (Int, Int) Light
sumBy f = sum . fmap f

changeLight :: ConwayBoard -> ((Int, Int), Light) -> ((Int, Int), Light)
changeLight board (i, light) = (i, next light (numberLighted i board))
    where 
        next _ _ 
            |isStuck i board = On
        next On 2 = On
        next On 3 = On
        next Off 3 = On
        next _ _ = Off
            
isStuck i = elem i . corners
        
corners board = [(a,a'),(a,b'), (b, a'), (b, b')]
    where 
        ((a, a'), (b, b')) = bounds board
        
   
numberLighted :: (Int, Int) -> ConwayBoard -> Int 
numberLighted i board = sumBy (\i -> toNumber $ board ! i) $ neighboorIndices i board

neighboorIndices (a, b) board = filter (/= (a,b)) $ range (min', max')
    where 
        ((minI, minJ), (maxI, maxJ)) = bounds board
        min' = (max minI (a - 1), max minJ (b - 1))
        max' = (min maxI (a + 1), min maxJ (b + 1))

step :: ConwayBoard -> ConwayBoard
step board = board // (map (changeLight board) $ assocs board)

countLights :: Int -> ConwayBoard -> Int
countLights n initialBoard =  sumBy toNumber $ (iterate step initialBoard !! n)

initBoard :: Int -> Int -> String -> ConwayBoard
initBoard i j str = array ((0,0), (i,j)) boardData
    where 
        boardData = concat $ zipWith (\i -> zipWith (\j x -> ((i,j), fromLetter x)) [0..j] ) [0..i] (lines str)

toString board = unlines . chunksOf (n+1) $ fmap toLetter $ elems board 
    where (_,(n,_)) = bounds board
        
example = array ((0,0),(5,5)) [((0,0),Off),((0,1),On),((0,2),Off),((0,3),On),((0,4),Off),((0,5),On),((1,0),Off),((1,1),Off),((1,2),Off),((1,3),On),((1,4),On),((1,5),Off),((2,0),On),((2,1),Off),((2,2),Off),((2,3),Off),((2,4),Off),((2,5),On),((3,0),Off),((3,1),Off),((3,2),On),((3,3),Off),((3,4),Off),((3,5),Off),((4,0),On),((4,1),Off),((4,2),On),((4,3),Off),((4,4),Off),((4,5),On),((5,0),On),((5,1),On),((5,2),On),((5,3),On),((5,4),Off),((5,5),Off)]
            
stuckCorners board = board // map (\i -> (i, On)) (corners board)
            
main = do
    board <- initBoard 99 99 <$> readFile "inputDay18.txt"
    print $ countLights 100 $ stuckCorners board