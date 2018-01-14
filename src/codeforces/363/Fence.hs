module Fence where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))

main :: IO ()
main = print =<< solve <$> ((!!1) . map read . words <$> getLine) <*> (map read . words <$> getLine)

solve :: Int -> [Int] -> Int
solve k xs = snd . minimum $ flip zip [1..] sumsOfEachKElements
  where
     sumsOfEachKElements = uncurry (zipWith (-)) pairWithAndWithoutK
     pairWithAndWithoutK = (drop k &&& id) progressivSums
     progressivSums = scanl (+) 0 xs

-- one line solution 
solve' :: Int -> [Int] -> Int
solve' k = snd . minimum . flip zip [1..] . uncurry (zipWith (-)) . (drop k &&& id) . scanl (+) 0
