module HR.SavePrincess2 where

{-
In this version of “Bot saves princess”, Princess Peach and
bot's position are randomly set. Can you save the princess?


Task

Complete the function nextMove which takes in 4 parameters - an integer n, an
integer x and y indicating the position of the bot and the character array grid
and output only the next move the bot makes to rescue the princess.


Input Format

The first line of the input is N (<100), the size of the board (NxN). The
second line of the input contains two space seperated integers, which is the
position of the bot in row-column format. Top left of the board is (0,0) and
the bottom right of the grid is (N-1,N-1). N lines follow each line containing
N characters which is the grid data.

The position of the princess is indicated by the character ‘p’
and the position of the bot is indicated by the character ‘m’
and each cell is denoted by ‘-’ (ascii value 45).


Output Format

Output only the next move you take to rescue the princess. Valid moves are LEFT
or RIGHT or UP or DOWN


Sample Input

5
2 2
-----
-----
p-m--
-----
-----

Sample Output

LEFT


Scoring

Your score for every testcase would be (NxN minus number of moves made to
rescue the princess)/10 where N is the size of the grid (5x5 in the sample
testcase).

-}


import           Control.Monad
import qualified Data.ByteString.Char8 as B8

data Move = L | D | U | R

instance Show Move where
  show L = "LEFT"
  show D = "DOWN"
  show U = "UP"
  show R = "RIGHT"

type Coord = (Int {- x -}, Int {- y -})

nextMoveToPrincess :: Coord -> Coord -> Move
nextMoveToPrincess (bx, by) (px, py) =
  case () of
   _ | px < bx   -> L
     | px > bx   -> R
     | py < by   -> U
     | py > by   -> D
     | otherwise -> error $ "nextMoveToPrincess: coord " ++ show (px,py)

getPrincessCoordFromGrid :: Int -> IO Coord
getPrincessCoordFromGrid n0
  | n0 == 0  = error "getPrincessCornerFromGrid: given n0 is 0"
  | even n0  = error "getPrincessCornerFromGrid: given n0 is even"
  | otherwise = go 0 where
  go y = do
    line <- B8.getLine
    case B8.elemIndex 'p' line of
      Nothing -> go (y + 1)
      Just x  -> replicateM_ (n0 - 1 - y) getLine >> return (x,y)

parseCoords :: String -> Coord
parseCoords s = let (y, (' ':x)) = break (==' ') s in
                (read x, read y)

main :: IO ()
main = do
    gridSize <- fmap read getLine
    botCoord <- fmap parseCoords getLine
    princessCoord <- getPrincessCoordFromGrid gridSize
    print $ nextMoveToPrincess botCoord princessCoord

