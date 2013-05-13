module HR.InsertionSort2 where


import           Control.Applicative
import           Control.Monad.Trans.Writer.Strict (tell, Writer, runWriter)
import           Data.Foldable (toList, foldlM)
import           Data.List (intersperse)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq


insertionSortTrace :: Seq Int -> Writer [String] (Seq Int)
insertionSortTrace s0 = foldlM ((dbg.).move) s0 [1..smax]
  where
    dbg s = tell [renderSeq s] >> return s

    smax = Seq.length s0 - 1
    move s i
      | i == 0 || i > smax      = s
      | iVal < pVal             = move pSwap p
      | otherwise               = s
      where
        iVal  = Seq.index s i -- value at i
        p     = i - 1 -- prev index
        pVal  = Seq.index s p -- prev value
        pSwap = Seq.update i pVal (Seq.update p iVal s) -- swap iVal with pVal


renderSeq :: Seq Int -> String
renderSeq = concat . intersperse " " . toList . fmap show

main :: IO ()
main = do
    len <- read <$> getLine
    s <- Seq.fromList . fmap read . take len . words <$> getLine
    let (_,msgs) = runWriter . insertionSortTrace $ s
    mapM_ putStrLn msgs
