module Fence where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.List

main :: IO ()
main = print =<< solve2 <$> ((!!1) . map read . words <$> getLine) <*> (map read . words <$> getLine)

solve :: Int -> [Int] -> Int
solve k xs = snd . minimum $ flip zip [1..] sumsOfEachKElements
  where
     sumsOfEachKElements = uncurry (zipWith (-)) pairWithAndWithoutK
     pairWithAndWithoutK = (drop k &&& id) progressivSums
     progressivSums = scanl (+) 0 xs

-- one line solution
solve' :: Int -> [Int] -> Int
solve' k = snd . minimum . flip zip [1..] . uncurry (zipWith (-)) . (drop k &&& id) . scanl (+) 0

-- another solution
solve2 :: Int -> [Int] -> Int
solve2 k xs = snd $ minimumBy cmpFst $ zip sums [1..]
  where
    sums = helper (sum $ take k xs) xs (drop k xs)
    helper s _ [] = [s]
    helper s (a:as) (b:bs) = s:helper (s-a+b) as bs

cmpFst (a,_) (b,_) = compare a b
