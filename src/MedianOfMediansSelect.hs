module MedianOfMediansSelect where

import Data.List
import Data.List.Split
import Control.Arrow ((&&&))

indexOfMedian :: Int -> Int
indexOfMedian n = (n - 1) `div` 2

bruteForceMedian :: Ord a => [a] -> a
bruteForceMedian xs = sort xs !! indexOfMedian (length xs)

pivot :: Ord a => a -> [a] -> ([a], [a])
pivot x = filter (< x) &&& filter (> x)


select :: (Ord a, Show a) => Int -> [a] -> a
select _ [] = error "Empty List"
select 0 [x] = x
select k xs | k < s = select k smaller
            | k == s = x
            | otherwise = select (k - s - 1) larger
            where medians = map bruteForceMedian $ chunksOf 5 xs
                  x = select (indexOfMedian $ length medians) medians 
                  (smaller, larger) = pivot x xs
                  s = length smaller
