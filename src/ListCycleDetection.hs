
module ListCycleDetection where

import Test.QuickCheck
import Test.QuickCheck.All

-- Method 1, Robert W. Floyd's algorithm
findCycle x0 f = (k, n) where
  (k, p) = connect x0 (converge (f x0) (f $ f x0)) 0
  n = traverse (f p)
  converge a b | a == b = a
               | otherwise = converge (f a) (f $ f b)
  connect a b cnt | a == b = (cnt, a)
                  | otherwise = connect (f a) (f b) (cnt + 1)
  traverse a | a == p = 1
             | otherwise = 1 + traverse (f a)


f k m n | n < k = n + 1
        | otherwise = (n + 1 - k) `mod` m + k

prop_cycle fcycle b c = (k, m) == fcycle 0 (f k m) where
  (k, m) = ((abs b) `mod` 1000, max 1 (abs c) `mod` 1000)   -- limit to 1000 to save the time

prop_floyd :: Int -> Int -> Bool
prop_floyd = prop_cycle findCycle


