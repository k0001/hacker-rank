module HR.InsertionSort1 where

{-
Sorting

One common tasks for computers is to sort data. For example, people might want
to see all their files on a computer sorted by size. Since sorting is a simple
problem with many different possible solutions, it is often used to introduce
the study of algorithms.

Insertion Sort

These challenges will cover Insertion Sort, a simple and intuitive sorting
algorithm. We will first start with an already sorted list.

Insert element into sorted list

Given a sorted list with an unsorted number V in the right-most cell, can you
write some simple code to insert V into the array so it remains sorted?

Print the array every time a value is shifted in the array until the array is
fully sorted. The goal of this challenge is to follow the correct order of
insertion sort.

Guideline: You can copy the value of V to a variable, and consider its cell
“empty”. Since this leaves an extra cell empty on the right, you
can shift everything over until V can be inserted. This will create a duplicate
of each value, but when you reach the right spot, you can replace a value with
V.

Input Format
There will be two lines of input:

 - s - the size of the array
 - ar - the sorted array of integers

Output Format
On each line, output the entire array every time an item is shifted in it.

Constraints
1<=s<=1000
-10000<=x<= 10000, x belongs to ar

Sample Input

5
2 4 6 8 3
Sample Output

2 4 6 8 8
2 4 6 6 8
2 4 4 6 8
2 3 4 6 8
Explanation

3 is removed from the end of the array.
In the 1st line 8 > 3, 8 is shifted one cell right.
In the 2nd line 6 > 3, 6 is shifted one cell right.
In the 3rd line 4 > 3, 4 is shifted one cell right.
In the 4th line 2 < 3, 3 is placed at position 2.

Task

Complete the method insertionSort which takes in 1 parameter:

ar - an array with the value V in the right-most cell.
Next Challenge

In the next Challenge, we will complete the insertion sort itself!

-}

import           Control.Applicative
import           Control.Monad.Trans.Writer.Strict (tell, Writer, runWriter)
import           Data.Foldable (toList)
import           Data.List (intersperse)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq


insertionSortTrace :: Seq Int -> Writer [String] (Seq Int)
insertionSortTrace s0 = move (len - 2) s0
  where
    len = Seq.length s0
    val = Seq.index s0 (len - 1)
    dbg s = tell [renderSeq s] >> return s
    move i s
      | i < 0      = dbg (Seq.update 0       val  s)
      | val < ival = dbg (Seq.update (i + 1) ival s) >>= move (i - 1)
      | otherwise  = dbg (Seq.update (i + 1) val  s)
      where
        ival = Seq.index s i

renderSeq :: Seq Int -> String
renderSeq = concat . intersperse " " . toList . fmap show

main :: IO ()
main = do
    len <- read <$> getLine
    s <- Seq.fromList . fmap read . take len . words <$> getLine
    let (_,msgs) = runWriter . insertionSortTrace $ s
    mapM_ putStrLn msgs


