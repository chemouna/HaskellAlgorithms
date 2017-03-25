module SelectionSort where

import Data.List

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]

selectionSort xs = selectionSort (delete x xs) ++ [x]
  where x = maximum xs

