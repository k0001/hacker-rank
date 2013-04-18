module HR.SavePrincess where

{-
Princess Peach is trapped in one of the four corners of a square grid. You
are in the center of the grid and can move one step at a time in any of the four
directions. Can you rescue the princess?

Input format

The first line contains an odd integer N (< 100) denoting the size of the
grid. This is followed by an NxN grid. Each cell is denoted by ‘-’ (ascii value:
45). The bot position is denoted by ‘m’ and the princess position is denoted by
‘p’.

The top left of the grid is indexed at (0,0) and the bottom right is indexed
at (N-1,N-1)


Output format

Print out all the moves you take to rescue the princess in one go. Moves
must be separated by ‘\n’ a newline. The valid outputs are LEFT or RIGHT or UP
or DOWN.


Sample input

3
---
-m-
p--


Sample output

DOWN
LEFT


Task

Complete the function displayPathtoPrincess which takes in two parameters -
the integer N and the character array grid. The function shall output moves
(LEFT, RIGHT, UP or DOWN) on consecutive lines to rescue/reach the princess. The
goal is to reach the princess in as few moves as possible

Scoring Your score is for every testcase would be: (NxN - moves made to
rescue the princess)/10, where N is the size of the grid (3x3 in the sample
testcase).
-}

import           Control.Monad
import qualified Data.ByteString.Char8 as B8

data Corner = LU | LD | RU | RD

data Move = L | D | U | R

instance Show Move where
  show L = "LEFT"
  show D = "DOWN"
  show U = "UP"
  show R = "RIGHT"

pathToPrinces :: Int -> Corner -> [Move]
pathToPrinces gridSize corner
  | even gridSize = error "pathToPrinces: given gridSize is even"
  | otherwise     = go $ gridSize `div` 2
  where
    go 0 = []
    go n = let rest = go (n - 1) in
           case corner of
             LU -> L : U : rest
             LD -> L : D : rest
             RU -> R : U : rest
             RD -> R : D : rest

getPrincessCornerFromGrid :: Int -> IO Corner
getPrincessCornerFromGrid n0
  | n0 == 0  = error "getPrincessCornerFromGrid: given n0 is 0"
  | even n0  = error "getPrincessCornerFromGrid: given n0 is even"
  | otherwise = go 0 where
  go n = do
    line <- B8.getLine
    let mr = case n of
         0  | B8.index line 0        == 'p' -> Just LU
            | B8.index line (n0 - 1) == 'p' -> Just RU
         _  | n < (n0 - 1)                  -> Nothing
            -- n >= (n0 - 1) below this line
            | B8.index line 0        == 'p' -> Just LD
            | B8.index line (n0 - 1) == 'p' -> Just RD
            | otherwise -> error $ "realGrid: line " ++ show n
    case mr of
      Nothing -> go (n + 1)
      Just r  -> replicateM_ (n0 - 1 - n) getLine >> return r

main :: IO ()
main = do
    gridSize <- fmap read getLine
    pc <- getPrincessCornerFromGrid gridSize
    mapM_ print $ pathToPrinces gridSize pc

