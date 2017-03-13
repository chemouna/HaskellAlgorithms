
module BucketSort where

import Data.Array

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
                 where n = length xs

bucketsort :: [Int] -> [Int]
bucketsort xs = concat [replicate k x | (x, k) <- assocs (countlist xs)]


