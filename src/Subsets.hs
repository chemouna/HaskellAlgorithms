
module Subsets where

import Data.List

perms [] = [[]]
perms xs = concatMap (\x -> (x:) <$> perms (delete x xs)) xs

subsets xs 0 = [[]]
subsets xs k
  | length xs < k = []
  | otherwise = subsets t k ++ map (h:) (subsets xs (k - 1))
    where (h:t) = xs

